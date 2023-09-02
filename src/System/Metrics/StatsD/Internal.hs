{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module System.Metrics.StatsD.Internal
  ( Stats (..),
    StatConfig (..),
    Key,
    Index,
    Sampling,
    Counter,
    Gauge,
    Timing,
    SetElement,
    Timings,
    SetData,
    MetricData (..),
    Store (..),
    Metrics,
    Value (..),
    Sample (..),
    Report (..),
    StatCounter (..),
    StatGauge (..),
    StatTiming (..),
    StatSet (..),
    addMetric,
    newMetric,
    validateKey,
    addReading,
    newReading,
    processSample,
    newStats,
    statsLoop,
    statsFlush,
    flushStats,
    catKey,
    statReports,
    TimingStats (..),
    makeTimingStats,
    extractPercentiles,
    timingReports,
    trimPercentile,
    percentileSuffix,
    timingStats,
    cumulativeSums,
    cumulativeSquares,
    stdev,
    mean,
    median,
    flush,
    toReport,
    format,
    submit,
    connectStatsD,
  )
where

import Control.Monad (forM_, forever, when)
import Data.ByteString.Char8 qualified as C
import Data.Char (isAlphaNum, isAscii)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (intercalate, sort)
import Network.Socket (Socket)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as Net
import Text.Printf (printf)
import UnliftIO (MonadIO, liftIO, throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    stateTVar,
  )

type Key = String

data Stats = Stats
  { metrics :: !(TVar Metrics),
    cfg :: !StatConfig,
    socket :: !Socket
  }

data StatConfig = StatConfig
  { reportStats :: !Bool,
    reportSamples :: !Bool,
    namespace :: !String,
    statsPrefix :: !String,
    prefixCounter :: !String,
    prefixTimer :: !String,
    prefixGauge :: !String,
    prefixSet :: !String,
    server :: !String,
    port :: !Int,
    flushInterval :: !Int,
    timingPercentiles :: ![Int],
    newline :: !Bool
  }
  deriving (Show, Read, Eq, Ord)

type Index = Int

type Sampling = Int

type Counter = Int

type Gauge = Int

type Timing = Int

type SetElement = String

type Timings = [Int]

type SetData = HashSet String

data MetricData
  = CounterData !Counter
  | GaugeData !Gauge
  | TimingData !Timings
  | SetData !(HashSet String)

data Store = Store
  { index :: !Index,
    dat :: !(Maybe MetricData)
  }

type Metrics = HashMap Key Store

data Value
  = Counter !Counter
  | Gauge !Gauge !Bool
  | Timing !Timing
  | Set !SetElement
  | Metric !Int
  | Other !String !String
  deriving (Eq, Ord, Show, Read)

data Report = Report
  { key :: !Key,
    value :: !Value,
    rate :: !Double
  }
  deriving (Eq, Ord, Show, Read)

data Sample = Sample
  { key :: !Key,
    value :: !Value,
    sampling :: !Sampling,
    index :: !Index
  }
  deriving (Eq, Ord, Show, Read)

data StatCounter = StatCounter
  { stats :: !Stats,
    key :: !Key,
    sampling :: !Sampling
  }

data StatGauge = StatGauge
  { stats :: !Stats,
    key :: !Key
  }

data StatTiming = StatTiming
  { stats :: !Stats,
    key :: !Key,
    sampling :: !Sampling
  }

data StatSet = StatSet
  { stats :: !Stats,
    key :: !Key
  }

addMetric :: StatConfig -> Key -> MetricData -> Metrics -> Metrics
addMetric cfg key md =
  HashMap.insert key $
    Store 0 $
      if cfg.reportStats
        then Just md
        else Nothing

newMetric :: (MonadIO m) => Stats -> Key -> MetricData -> m ()
newMetric stats key store
  | validateKey key = do
      e <- atomically $ do
        exists <- HashMap.member key <$> readTVar stats.metrics
        if exists
          then return True
          else do
            modifyTVar stats.metrics (addMetric stats.cfg key store)
            return False
      when e $
        throwIO $
          userError $
            "A metric already exists with key: " <> key
  | otherwise =
      throwIO $ userError $ "Metric key is invalid: " <> key

validateKey :: String -> Bool
validateKey t = not (null t) && all valid t
  where
    valid c = elem c ("._-" :: [Char]) || isAscii c && isAlphaNum c

addReading :: Value -> Key -> Metrics -> Metrics
addReading reading = HashMap.adjust adjust
  where
    adjust m = m {index = m.index + 1, dat = change <$> m.dat}
    change store = case (reading, store) of
      (Counter c, CounterData s) -> CounterData (s + c)
      (Gauge i False, GaugeData _) -> GaugeData i
      (Gauge i True, GaugeData g) -> GaugeData (max 0 (g + i))
      (Timing t, TimingData s) -> TimingData (t : s)
      (Set e, SetData s) -> SetData (HashSet.insert e s)
      _ -> error "Stats reading mismatch"

newReading :: Stats -> Key -> Value -> STM Int
newReading stats key reading = do
  modifyTVar stats.metrics (addReading reading key)
  maybe 0 (.index) . HashMap.lookup key <$> readTVar stats.metrics

processSample ::
  (MonadIO m) => Stats -> Sampling -> Key -> Value -> m ()
processSample stats sampling key val = do
  idx <- atomically $ newReading stats key val
  when stats.cfg.reportSamples $
    submit stats $
      Sample key val sampling idx

newStats :: (MonadIO m) => StatConfig -> m Stats
newStats cfg = do
  m <- newTVarIO HashMap.empty
  h <- connectStatsD cfg.server cfg.port
  return $ Stats m cfg h

statsLoop :: (MonadIO m) => Stats -> m ()
statsLoop stats = forever $ do
  threadDelay $ stats.cfg.flushInterval * 1000
  statsFlush stats

statsFlush :: (MonadIO m) => Stats -> m ()
statsFlush stats = do
  reports <-
    atomically $
      stateTVar stats.metrics (flushStats stats.cfg)
  mapM_ (send stats) reports

flushStats :: StatConfig -> Metrics -> ([Report], Metrics)
flushStats cfg metrics =
  let f xs key m = maybe xs ((<> xs) . statReports cfg key) m.dat
      rs = HashMap.foldlWithKey' f [] metrics
      g m = m {dat = flush <$> m.dat}
      ms = HashMap.map g metrics
   in (rs, ms)

catKey :: [Key] -> Key
catKey = intercalate "." . filter (not . null)

statReports :: StatConfig -> Key -> MetricData -> [Report]
statReports cfg key dat = case dat of
  CounterData c ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixCounter, key, "count"],
          value = Counter c,
          rate = 1.0
        },
      Report
        { key = catKey [cfg.statsPrefix, cfg.prefixCounter, key, "rate"],
          value = Counter (computeRate cfg c),
          rate = 1.0
        }
    ]
  GaugeData s ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixGauge, key],
          value = Gauge s False,
          rate = 1.0
        }
    ]
  SetData s ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixSet, key, "count"],
          value = Counter (HashSet.size s),
          rate = 1.0
        }
    ]
  TimingData s -> timingReports cfg key s

data TimingStats = TimingStats
  { timings :: ![Int],
    cumsums :: ![Int],
    cumsquares :: ![Int]
  }
  deriving (Eq, Ord, Show, Read)

makeTimingStats :: Timings -> TimingStats
makeTimingStats timings =
  TimingStats
    { timings = sorted,
      cumsums = cumulativeSums sorted,
      cumsquares = cumulativeSquares sorted
    }
  where
    sorted = sort timings

extractPercentiles :: StatConfig -> [Int]
extractPercentiles =
  HashSet.toList
    . HashSet.fromList
    . filter (\x -> x > 0 && x < 100)
    . (.timingPercentiles)

timingReports :: StatConfig -> Key -> Timings -> [Report]
timingReports cfg key timings =
  concatMap (timingStats cfg key tstats) percentiles
  where
    tstats = makeTimingStats timings
    percentiles = 100 : extractPercentiles cfg

trimPercentile :: Int -> TimingStats -> TimingStats
trimPercentile pc ts =
  ts
    { timings = f ts.timings,
      cumsums = f ts.cumsums,
      cumsquares = f ts.cumsquares
    }
  where
    f ls = take (length ls * pc `div` 100) ls

percentileSuffix :: Int -> String
percentileSuffix pc
  | 100 <= pc = ""
  | 0 > pc = "0"
  | otherwise = "_" <> show pc

computeRate :: StatConfig -> Int -> Int
computeRate cfg i =
  round (fromIntegral i * 1000.0 / fromIntegral cfg.flushInterval :: Double)

mean :: TimingStats -> Int
mean ts = last ts.cumsums `div` length ts.timings

timingStats :: StatConfig -> Key -> TimingStats -> Int -> [Report]
timingStats cfg key tstats pc =
  mkr "count" (Counter (length ts.timings))
    : [mkr "count_ps" (Counter rate) | 100 <= pc]
      <> if null ts.timings
        then []
        else stats
  where
    k s =
      catKey
        [ cfg.statsPrefix,
          cfg.prefixTimer,
          key,
          s <> percentileSuffix pc
        ]
    ts = trimPercentile pc tstats
    rate = computeRate cfg (length ts.timings)
    mkr s v = Report {key = k s, value = v, rate = 1.0}
    stats =
      [ mkr "mean" (Timing (mean ts)),
        mkr "upper" (Timing (last ts.timings)),
        mkr "lower" (Timing (head ts.timings)),
        mkr "sum" (Timing (last ts.cumsums)),
        mkr "sum_squares" (Timing (last ts.cumsquares)),
        mkr "median" (Timing (median ts))
      ]
        <> [ mkr "std" (Timing (stdev ts))
             | 100 <= pc
           ]

cumulativeSums :: (Num a) => [a] -> [a]
cumulativeSums = scanl1 (+)

cumulativeSquares :: (Num a) => [a] -> [a]
cumulativeSquares = scanl1 (+) . map (\x -> x * x)

stdev :: TimingStats -> Int
stdev ts =
  round $ sqrt var
  where
    len = length ts.timings
    var = fromIntegral diffsum / fromIntegral len :: Double
    diffsum = sum $ map ((^ (2 :: Int)) . subtract (mean ts)) ts.timings

median :: TimingStats -> Int
median ts
  | null ts.timings = 0
  | even (length ts.timings) =
      let lower = ts.timings !! middle
          upper = ts.timings !! subtract 1 middle
       in (lower + upper) `div` 2
  | otherwise =
      ts.timings !! middle
  where
    middle = length ts.timings `div` 2

flush :: MetricData -> MetricData
flush (CounterData _) = CounterData 0
flush (GaugeData g) = GaugeData g
flush (TimingData _) = TimingData []
flush (SetData _) = SetData HashSet.empty

toReport :: Sample -> Maybe Report
toReport sample
  | sample.sampling > 0 && sample.index `mod` sample.sampling == 0 =
      Just
        Report
          { key = sample.key,
            value = sample.value,
            rate = 1.0 / fromIntegral sample.sampling
          }
  | otherwise = Nothing

format :: StatConfig -> Report -> String
format cfg report
  | cfg.newline = printf "%s:%s\n" key val
  | otherwise = printf "%s:%s" key val
  where
    key = catKey [cfg.namespace, report.key]
    rate
      | report.rate < 1.0 = printf "|@%f" report.rate
      | otherwise = "" :: String
    val =
      case report.value of
        Counter i ->
          printf "%d|c%s" i rate
        Gauge g False ->
          printf "%d|g" g
        Gauge g True ->
          printf "%+d|g" g
        Timing t ->
          printf "%d|ms%s" t rate
        Set e ->
          printf "%s|s" e
        Metric m ->
          printf "%s|m" m
        Other d t ->
          printf "%s|%s" t d :: String

submit :: (MonadIO m) => Stats -> Sample -> m ()
submit stats sample =
  forM_ (toReport sample) (send stats)

send :: (MonadIO m) => Stats -> Report -> m ()
send stats report =
  liftIO $
    Net.sendAll stats.socket $
      C.pack $
        format stats.cfg report

connectStatsD :: (MonadIO m) => String -> Int -> m Socket
connectStatsD host port = liftIO $ do
  as <-
    Net.getAddrInfo
      Nothing
      (Just host)
      (Just $ show port)
  a <- case as of
    a : _ -> return a
    [] -> error $ "Cannot resolve: " <> host <> ":" <> show port
  sock <- Net.socket (Net.addrFamily a) Net.Datagram Net.defaultProtocol
  Net.connect sock (Net.addrAddress a)
  return sock