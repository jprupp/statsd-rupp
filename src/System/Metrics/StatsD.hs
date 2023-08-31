{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module System.Metrics.StatsD
  ( StatCounter,
    StatGauge,
    StatTiming,
    StatSet,
    Stats,
    StatConfig (..),
    newStatCounter,
    newStatGauge,
    newStatTiming,
    newStatSet,
    incrementCounter,
    setGauge,
    addTiming,
    newSetElement,
    withStats,
  )
where

import Control.Monad (forever, when)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isAscii)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (intercalate, sort)
import Network.Socket qualified as Net
import System.IO (Handle, IOMode (..), hPutStr)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Async (withAsync)
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
    handle :: !Handle
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
    timingPercentiles :: ![Int]
  }

type Index = Int

type Sampling = Int

type Counter = Int

type Gauge = Int

type Timing = Int

type Element = String

type Timings = [Int]

type Set = HashSet String

data Store
  = StoreCounter !Counter
  | StoreGauge !Gauge
  | StoreTimings !Timings
  | StoreSet !Set

data Metric = Metric
  { index :: !Index,
    store :: !(Maybe Store)
  }

type Metrics = HashMap Key Metric

data Value
  = Counter !Counter
  | Gauge !Gauge
  | Timing !Timing
  | Set !Element
  deriving (Eq, Ord, Show, Read)

data Report = Report
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
    key :: !Key,
    sampling :: !Sampling
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

addMetric :: StatConfig -> Key -> Store -> Metrics -> Metrics
addMetric cfg key store =
  HashMap.insert key $
    Metric 0 $
      if cfg.reportStats
        then Just store
        else Nothing

newMetric :: (MonadIO m) => Stats -> Key -> Store -> m Bool
newMetric stats key store
  | validateKey key =
      atomically $ do
        exists <- HashMap.member key <$> readTVar stats.metrics
        if exists
          then return False
          else do
            modifyTVar stats.metrics (addMetric stats.cfg key store)
            return True
  | otherwise = return False

newStatCounter ::
  (MonadIO m) => Stats -> Key -> Sampling -> m (Maybe StatCounter)
newStatCounter stats key sampling = do
  success <- newMetric stats key (StoreCounter 0)
  if success
    then return $ Just $ StatCounter stats key sampling
    else return Nothing

newStatGauge ::
  (MonadIO m) => Stats -> Key -> Int -> Int -> m (Maybe StatGauge)
newStatGauge stats key sampling ini = do
  success <- newMetric stats key (StoreGauge ini)
  if success
    then return $ Just $ StatGauge stats key sampling
    else return Nothing

newStatTiming :: (MonadIO m) => Stats -> Key -> Int -> m (Maybe StatTiming)
newStatTiming stats key sampling = do
  success <- newMetric stats key (StoreTimings [])
  if success
    then return $ Just $ StatTiming stats key sampling
    else return Nothing

newStatSet :: (MonadIO m) => Stats -> Key -> m (Maybe StatSet)
newStatSet stats key = do
  success <- newMetric stats key (StoreSet HashSet.empty)
  if success
    then return $ Just $ StatSet stats key
    else return Nothing

validateKey :: String -> Bool
validateKey t = not (null t) && all valid t
  where
    valid c = elem c ("._-" :: [Char]) || isAscii c && isAlphaNum c

addReading :: Value -> Key -> Metrics -> Metrics
addReading reading = HashMap.adjust adjust
  where
    adjust m = m {index = m.index + 1, store = change <$> m.store}
    change store = case (reading, store) of
      (Counter c, StoreCounter s) -> StoreCounter (s + c)
      (Gauge i, StoreGauge _) -> StoreGauge i
      (Timing t, StoreTimings s) -> StoreTimings (t : s)
      (Set e, StoreSet s) -> StoreSet (HashSet.insert e s)
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
      Report key val sampling idx

incrementCounter :: (MonadIO m) => StatCounter -> Int -> m ()
incrementCounter StatCounter {..} =
  processSample stats sampling key . Counter

setGauge :: (MonadIO m) => StatGauge -> Int -> m ()
setGauge StatGauge {..} =
  processSample stats sampling key . Gauge

addTiming :: (MonadIO m) => StatTiming -> Int -> m ()
addTiming StatTiming {..} =
  processSample stats sampling key . Gauge

newSetElement :: (MonadIO m) => StatSet -> String -> m ()
newSetElement StatSet {..} =
  processSample stats 1 key . Set

withStats :: (MonadUnliftIO m) => StatConfig -> (Stats -> m a) -> m a
withStats cfg go = do
  stats <- newStats cfg
  if cfg.reportStats
    then withAsync (statsLoop stats) (const (go stats))
    else go stats

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
  mapM_ (submit stats) reports

flushStats :: StatConfig -> Metrics -> ([Report], Metrics)
flushStats cfg metrics =
  let f xs key m = maybe xs ((<> xs) . statReports cfg key) m.store
      rs = HashMap.foldlWithKey' f [] metrics
      g m = m {store = flush <$> m.store}
      ms = HashMap.map g metrics
   in (rs, ms)

catKey :: [Key] -> Key
catKey = intercalate "." . filter (not . null)

statReports :: StatConfig -> Key -> Store -> [Report]
statReports cfg key store = case store of
  StoreCounter c ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixCounter, key, "count"],
          value = Counter c,
          sampling = 1,
          index = 0
        },
      Report
        { key = catKey [cfg.statsPrefix, cfg.prefixCounter, key, "rate"],
          value = Counter (c `div` (cfg.flushInterval * 1000)),
          sampling = 1,
          index = 0
        }
    ]
  StoreGauge s ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixGauge, key],
          value = Gauge s,
          sampling = 1,
          index = 0
        }
    ]
  StoreSet s ->
    [ Report
        { key = catKey [cfg.statsPrefix, cfg.prefixSet, key, "count"],
          value = Counter (HashSet.size s),
          sampling = 1,
          index = 0
        }
    ]
  StoreTimings s -> timingReports cfg key s

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
    rate = length ts.timings `div` (cfg.flushInterval * 1000)
    mkr s v = Report {key = k s, value = v, sampling = 1, index = 0}
    stats =
      [ mkr "mean" (Timing (last ts.cumsums `div` length ts.timings)),
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
cumulativeSquares = scanl1 (+) . map (^ (2 :: Int))

stdev :: TimingStats -> Int
stdev ts =
  round $ sqrt $ diffsum / mean
  where
    cumsum = fromIntegral $ last ts.cumsums :: Double
    len = fromIntegral $ length ts.timings :: Double
    mean = cumsum / len
    diff x =
      let d = fromIntegral x - mean :: Double
       in d * d
    diffsum = sum $ map diff ts.timings

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

flush :: Store -> Store
flush (StoreCounter _) = StoreCounter 0
flush (StoreGauge g) = StoreGauge g
flush (StoreTimings _) = StoreTimings []
flush (StoreSet _) = StoreSet HashSet.empty

submit :: (MonadIO m) => Stats -> Report -> m ()
submit stats report =
  when sendit $ liftIO $ hPutStr stats.handle $ pfx <> str
  where
    pfx = bool (stats.cfg.namespace <> ".") "" $ null stats.cfg.namespace
    sendit = report.sampling > 0 && report.index `mod` report.sampling == 0
    rate i
      | i > 1 = "|@" <> show (1.0 / fromIntegral i :: Double)
      | otherwise = ""
    str =
      report.key <> ":" <> case report.value of
        Counter i -> show i <> "|c" <> rate report.sampling
        Gauge g -> show g <> "|ms" <> rate report.sampling
        Timing t -> show t <> "|g"
        Set e -> show e <> "|s"

connectStatsD :: (MonadIO m) => String -> Int -> m Handle
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
  Net.socketToHandle sock WriteMode