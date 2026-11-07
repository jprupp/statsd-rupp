{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : System.Metrics.StatsD.Internal
-- Copyright   : (c) 2023 JP Rupp
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation details of "System.Metrics.StatsD". This module is exposed for
-- testing and for advanced use. Its API may change between minor versions.
--
-- Metrics are kept in a 'HashMap' from key to 'Store' inside a 'TVar'. Every
-- reading updates the aggregated 'MetricData' of its metric and increments a
-- per-metric counter, which is used to decide which samples to send when
-- sampling. A background loop ('statsLoop') periodically turns the aggregated
-- data into 'Report's, sends them, and resets the data ('flush').
--
-- Keeping every metric in one 'TVar' means a reading is a single short
-- transaction, and a flush can take a consistent picture of all metrics at once
-- and reset them atomically, so no reading is counted twice or lost between
-- intervals.
--
-- The parser ('parseReport') is not used by the client. It exists so that the
-- tests can check that every line the client formats reads back as the same
-- report.
module System.Metrics.StatsD.Internal
  ( Stats (..),
    newStats,
    StatParams,
    newParams,
    StatConfig (..),
    MetricData (..),
    Store (..),
    Metrics,
    newMetrics,
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
    statsLoop,
    statsFlush,
    flushStats,
    catKey,
    statReports,
    TimingStats (..),
    makeTimingStats,
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
    formatReport,
    submit,
    connectStatsD,
    parseReport,
    parseRead,
    parseInt,
  )
where

import Control.Monad (MonadPlus (..), forM_, forever, guard, unless, void, when)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (byteString, char8, intDec, string8, toLazyByteString)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as L
import Data.Char (isAlphaNum, isAscii)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (nub, sort)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Network.Socket (Socket)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as Net
import Text.Printf (printf)
import Text.Read (readMaybe)
import UnliftIO (MonadIO, handleIO, liftIO, throwIO)
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

-- | Handle to a StatsD client. Create one with
-- 'System.Metrics.StatsD.withStats'. It can be shared between threads.
data Stats = Stats
  { -- | registered metrics and their aggregated data
    metrics :: !(TVar Metrics),
    -- | UDP socket connected to the StatsD server
    socket :: !Socket,
    -- | settings derived from the 'StatConfig'
    params :: !StatParams
  }

-- | Settings derived from a 'StatConfig' by 'newParams', with the prefixes
-- already joined and encoded.
data StatParams = StatParams
  { -- | namespace followed by a dot, or empty; prepended to samples
    pfx :: !ByteString,
    -- | full prefix for counter statistics, ending in a dot
    pfxCounter :: !ByteString,
    -- | full prefix for timing statistics, ending in a dot
    pfxTimer :: !ByteString,
    -- | full prefix for gauge statistics, ending in a dot
    pfxGauge :: !ByteString,
    -- | full prefix for set statistics, ending in a dot
    pfxSet :: !ByteString,
    -- | flush interval in milliseconds
    flush :: !Int,
    -- | whether aggregated statistics are kept and reported
    stats :: !Bool,
    -- | whether individual samples are reported
    samples :: !Bool,
    -- | percentiles to compute for timings, always including 100
    percentiles :: ![Int],
    -- | whether to append a newline to each line sent
    newline :: !Bool
  }

-- | Client configuration. Start from 'System.Metrics.StatsD.defStatConfig' and
-- change the fields you need.
data StatConfig = StatConfig
  { -- | aggregate samples and send statistics every 'flushInterval'
    reportStats :: !Bool,
    -- | send every sample (subject to sampling) as it happens
    reportSamples :: !Bool,
    -- | prefix for every metric name; empty for none
    namespace :: !String,
    -- | prefix for aggregated statistics, after the namespace
    prefixStats :: !String,
    -- | prefix for counter statistics, after 'prefixStats'
    prefixCounter :: !String,
    -- | prefix for timing statistics, after 'prefixStats'
    prefixTimer :: !String,
    -- | prefix for gauge statistics, after 'prefixStats'
    prefixGauge :: !String,
    -- | prefix for set statistics, after 'prefixStats'
    prefixSet :: !String,
    -- | host name or IP address of the StatsD server
    statsdServer :: !String,
    -- | UDP port of the StatsD server
    statsdPort :: !Int,
    -- | milliseconds between flushes of aggregated statistics; must be
    -- positive
    flushInterval :: !Int,
    -- | percentiles to compute for timings, each strictly between 0 and 100
    timingPercentiles :: ![Int],
    -- | append a newline to each line sent, as some servers require
    appendNewline :: !Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | Aggregated data of a metric during the current flush interval.
data MetricData
  = -- | sum of increments
    CounterData !Int
  | -- | current value
    GaugeData !Int
  | -- | timings recorded, most recent first
    TimingData ![Int]
  | -- | distinct elements seen
    SetData !(HashSet ByteString)

-- | State of a registered metric.
data Store = Store
  { -- | number of readings so far, used for sampling
    index :: !Int,
    -- | aggregated data, or 'Nothing' if statistics are not reported
    dat :: !(Maybe MetricData)
  }

-- | All registered metrics, by key.
type Metrics = HashMap ByteString Store

-- | A single reading, or the value of a report.
data Value
  = -- | counter increment (@|c@)
    Counter !Int
  | -- | gauge value; if the flag is 'True' it is a relative change and is sent
    -- with an explicit sign (@|g@)
    Gauge !Int !Bool
  | -- | timing in milliseconds (@|ms@)
    Timing !Int
  | -- | set element (@|s@)
    Set !ByteString
  deriving (Eq, Ord, Show, Read)

-- | A line to send to the StatsD server, in the form
-- @key:value|type[|\@rate]@.
data Report = Report
  { -- | full metric name
    key :: !ByteString,
    -- | value and type
    value :: !Value,
    -- | sampling rate between 0 and 1; only sent if below 1
    rate :: !Double
  }
  deriving (Eq, Ord, Show, Read)

-- | A reading together with what is needed to decide whether to send it.
data Sample = Sample
  { -- | metric key, without namespace
    key :: !ByteString,
    -- | the reading
    value :: !Value,
    -- | send one in this many readings; @0@ to send none
    sampling :: !Int,
    -- | number of readings of this metric so far, including this one
    index :: !Int
  }
  deriving (Eq, Ord, Show, Read)

-- | Handle to a counter. Create with 'System.Metrics.StatsD.newStatCounter'.
data StatCounter = StatCounter
  { -- | client the counter belongs to
    stats :: !Stats,
    -- | metric key
    key :: !ByteString,
    -- | sampling rate for individual samples
    sampling :: !Int
  }

-- | Handle to a gauge. Create with 'System.Metrics.StatsD.newStatGauge'.
data StatGauge = StatGauge
  { -- | client the gauge belongs to
    stats :: !Stats,
    -- | metric key
    key :: !ByteString
  }

-- | Handle to a timing metric. Create with
-- 'System.Metrics.StatsD.newStatTiming'.
data StatTiming = StatTiming
  { -- | client the timing belongs to
    stats :: !Stats,
    -- | metric key
    key :: !ByteString,
    -- | sampling rate for individual samples
    sampling :: !Int
  }

-- | Handle to a set. Create with 'System.Metrics.StatsD.newStatSet'.
data StatSet = StatSet
  { -- | client the set belongs to
    stats :: !Stats,
    -- | metric key
    key :: !ByteString
  }

-- | Add a metric with its initial data to the map. The data is only kept if
-- statistics are reported.
addMetric :: StatParams -> ByteString -> MetricData -> Metrics -> Metrics
addMetric params key dat =
  HashMap.insert key $
    Store 0 $
      if params.stats then Just dat else Nothing

-- | Register a metric. Throws an 'IOError' if the key is invalid or already
-- registered.
newMetric :: (MonadIO m) => Stats -> ByteString -> MetricData -> m ()
newMetric stats key store = do
  unless (validateKey key) $ do
    throwIO $ userError "Metric key is invalid"
  e <- atomically $ do
    exists <- HashMap.member key <$> readTVar stats.metrics
    if exists
      then return True
      else do
        modifyTVar
          stats.metrics
          (addMetric stats.params key store)
        return False
  when e $ throwIO $ userError $ "StatsD key exists: " <> C.unpack key

-- | A key is valid if it is not empty and only contains ASCII letters, digits,
-- @.@, @_@ and @-@.
validateKey :: ByteString -> Bool
validateKey t = not (C.null t) && C.all valid t
  where
    valid c = elem c ("._-" :: [Char]) || isAscii c && isAlphaNum c

-- | Apply a reading to the aggregated data of a metric and increment its reading
-- count. Relative gauge changes saturate at 'maxBound' and never go below zero.
-- Calls 'error' if the reading does not match the type of the metric.
--
-- Gauges saturate rather than wrap around because a gauge that suddenly jumps
-- from a huge value to zero, or below, would be worse than one stuck at its
-- maximum.
addReading :: Value -> ByteString -> Metrics -> Metrics
addReading reading = HashMap.adjust adjust
  where
    adjust m = m {index = m.index + 1, dat = change <$> m.dat}
    change store = case (reading, store) of
      (Counter c, CounterData s) ->
        CounterData (s + c)
      (Gauge i False, GaugeData _) ->
        GaugeData i
      (Gauge i True, GaugeData g)
        | i > maxBound - g -> GaugeData maxBound
        | otherwise -> GaugeData (max 0 (g + i))
      (Timing t, TimingData s) ->
        TimingData (t : s)
      (Set e, SetData s) ->
        SetData (HashSet.insert e s)
      _ -> error "Stats reading mismatch"

-- | Apply a reading and return the updated reading count of the metric, or
-- @0@ if the metric does not exist.
newReading :: Stats -> ByteString -> Value -> STM Int
newReading stats key reading = do
  modifyTVar stats.metrics (addReading reading key)
  maybe 0 (.index) . HashMap.lookup key
    <$> readTVar stats.metrics

-- | Record a reading and, if samples are reported, send it subject to the
-- sampling rate.
processSample ::
  (MonadIO m) => Stats -> Int -> ByteString -> Value -> m ()
processSample stats sampling key val = do
  idx <- atomically $ newReading stats key val
  when stats.params.samples $
    submit stats $
      Sample key val sampling idx

-- | Create an empty set of metrics.
newMetrics :: (MonadIO m) => m (TVar Metrics)
newMetrics = newTVarIO HashMap.empty

-- | Validate a configuration and derive its parameters. Calls 'error' if the
-- configuration is invalid.
newParams :: StatConfig -> StatParams
newParams cfg
  | valid =
      StatParams
        { pfx = pfx_namespace,
          pfxCounter = full_pfx_stats <> counters <> ".",
          pfxGauge = full_pfx_stats <> gauges <> ".",
          pfxTimer = full_pfx_stats <> timers <> ".",
          pfxSet = full_pfx_stats <> sets <> ".",
          newline = cfg.appendNewline,
          stats = cfg.reportStats,
          samples = cfg.reportSamples,
          percentiles = 100 : nub cfg.timingPercentiles,
          flush = cfg.flushInterval
        }
  | otherwise = error "StatsD config invalid"
  where
    namespace = C.pack cfg.namespace
    pfx_stats = C.pack cfg.prefixStats
    gauges = C.pack cfg.prefixGauge
    counters = C.pack cfg.prefixCounter
    timers = C.pack cfg.prefixTimer
    sets = C.pack cfg.prefixSet
    valid =
      all validateKey [pfx_stats, gauges, counters, timers, sets]
        && bool (validateKey namespace) True (C.null namespace)
        && cfg.flushInterval > 0
        && all (\pc -> pc > 0 && 100 > pc) cfg.timingPercentiles
    pfx_namespace =
      if C.null namespace
        then ""
        else namespace <> "."
    full_pfx_stats = pfx_namespace <> pfx_stats <> "."

-- | Build a client handle from its parts.
newStats :: StatConfig -> TVar Metrics -> Socket -> Stats
newStats cfg metrics socket =
  Stats
    { metrics = metrics,
      socket = socket,
      params = newParams cfg
    }

-- | Flush statistics every flush interval, forever.
statsLoop :: (MonadIO m) => Stats -> m ()
statsLoop stats = forever $ do
  threadDelay (stats.params.flush * 1000)
  statsFlush stats

-- | Send the statistics of all metrics and reset them.
statsFlush :: (MonadIO m) => Stats -> m ()
statsFlush stats = do
  mapM_ (send stats)
    =<< atomically
      (stateTVar stats.metrics (flushStats stats.params))

-- | Compute the reports for all metrics and the metrics reset for the next
-- interval.
flushStats :: StatParams -> Metrics -> ([Report], Metrics)
flushStats params metrics =
  let f xs key m = maybe xs ((<> xs) . statReports params key) m.dat
      rs = HashMap.foldlWithKey' f [] metrics
      g m = m {dat = flush <$> m.dat}
      ms = HashMap.map g metrics
   in (rs, ms)

-- | Join non-empty key parts with dots.
catKey :: [ByteString] -> ByteString
catKey = C.intercalate "." . filter (not . C.null)

-- | Reports for the aggregated data of a metric.
statReports :: StatParams -> ByteString -> MetricData -> [Report]
statReports params key dat = case dat of
  CounterData c ->
    [ Report
        { key = params.pfxCounter <> key <> ".count",
          value = Counter c,
          rate = 1.0
        },
      Report
        { key = params.pfxCounter <> key <> ".rate",
          value = Counter (computeRate params c),
          rate = 1.0
        }
    ]
  GaugeData s ->
    [ Report
        { key = params.pfxGauge <> key,
          value = Gauge s False,
          rate = 1.0
        }
    ]
  SetData s ->
    [ Report
        { key = params.pfxSet <> key <> ".count",
          value = Counter (HashSet.size s),
          rate = 1.0
        }
    ]
  TimingData s -> timingReports params key s

-- | Sorted timings with precomputed running totals, so that statistics for
-- any percentile can be read off a prefix of the vectors.
data TimingStats = TimingStats
  { -- | timings in ascending order
    timings :: !(Vector Int),
    -- | running sums of the timings
    cumsums :: !(Vector Int),
    -- | running sums of the squares of the timings
    cumsquares :: !(Vector Int)
  }
  deriving (Eq, Ord, Show, Read)

-- | Sort timings and compute their running totals.
--
-- Sorting once and keeping running sums lets every percentile's count, sum,
-- mean and sum of squares be read from a prefix of the vectors in constant time,
-- instead of recomputing them for each percentile.
makeTimingStats :: [Int] -> TimingStats
makeTimingStats timings =
  TimingStats
    { timings = V.fromList sorted,
      cumsums = V.fromList (cumulativeSums sorted),
      cumsquares = V.fromList (cumulativeSquares sorted)
    }
  where
    sorted = sort timings

-- | Reports for a timing metric, for every configured percentile.
timingReports :: StatParams -> ByteString -> [Int] -> [Report]
timingReports params key timings =
  concatMap (timingStats params key tstats) params.percentiles
  where
    tstats = makeTimingStats timings

-- | Keep only the lowest given percentage of the timings.
trimPercentile :: Int -> TimingStats -> TimingStats
trimPercentile pc ts =
  ts
    { timings = f ts.timings,
      cumsums = f ts.cumsums,
      cumsquares = f ts.cumsquares
    }
  where
    f ls = V.take (length ls * pc `div` 100) ls

-- | Suffix for statistics of a percentile: empty for 100, @_N@ otherwise.
percentileSuffix :: Int -> ByteString
percentileSuffix pc
  | pc == 100 = ""
  | otherwise = C.pack $ printf "_%d" pc

-- | Turn a count over the flush interval into a count per second.
computeRate :: StatParams -> Int -> Int
computeRate params i = i * 1000 `div` params.flush

-- | Integer mean of the timings. Must not be called on empty stats.
mean :: TimingStats -> Int
mean ts = V.last ts.cumsums `div` length ts.timings

-- | Reports for a timing metric at one percentile.
timingStats :: StatParams -> ByteString -> TimingStats -> Int -> [Report]
timingStats params key tstats pc =
  concat
    [ [mkr "count" (Counter (length ts.timings))],
      [mkr "count_ps" (Counter rate) | pc == 100],
      [mkr "std" (Timing (stdev ts)) | pc == 100, not empty],
      [mkr "mean" (Timing (mean ts)) | not empty],
      [mkr "upper" (Timing (V.last ts.timings)) | not empty],
      [mkr "lower" (Timing (V.head ts.timings)) | not empty],
      [mkr "sum" (Timing (V.last ts.cumsums)) | not empty],
      [mkr "sum_squares" (Timing (V.last ts.cumsquares)) | not empty],
      [mkr "median" (Timing (median ts)) | not empty]
    ]
  where
    ts = trimPercentile pc tstats
    empty = null ts.timings
    rate = computeRate params (length ts.timings)
    sfx = percentileSuffix pc
    pfx = params.pfxTimer <> key <> "."
    mkr s v = Report {key = pfx <> s <> sfx, value = v, rate = 1.0}

-- | Running sums of a list.
cumulativeSums :: (Num a) => [a] -> [a]
cumulativeSums = scanl1 (+)

-- | Running sums of the squares of a list.
cumulativeSquares :: (Num a) => [a] -> [a]
cumulativeSquares = scanl1 (+) . map (\x -> x * x)

-- | Population standard deviation of the timings, rounded.
stdev :: TimingStats -> Int
stdev ts =
  round $ sqrt var
  where
    len = length ts.timings
    var = fromIntegral ds / fromIntegral len :: Double
    ds = sum $ map ((^ (2 :: Int)) . subtract (mean ts)) (V.toList ts.timings)

-- | Median of the timings, or @0@ if there are none.
median :: TimingStats -> Int
median ts
  | null ts.timings = 0
  | even (length ts.timings) =
      let lower = ts.timings ! middle
          upper = ts.timings ! subtract 1 middle
       in (lower + upper) `div` 2
  | otherwise =
      ts.timings ! middle
  where
    middle = length ts.timings `div` 2

-- | Reset aggregated data for the next interval. Gauges keep their value.
flush :: MetricData -> MetricData
flush (CounterData _) = CounterData 0
flush (GaugeData g) = GaugeData g
flush (TimingData _) = TimingData []
flush (SetData _) = SetData HashSet.empty

-- | Turn a sample into a report if the sampling rate selects it.
toReport :: StatParams -> Sample -> Maybe Report
toReport params sample
  | sample.sampling > 0 && sample.index `mod` sample.sampling == 0 =
      Just
        Report
          { key = params.pfx <> sample.key,
            value = sample.value,
            rate = 1.0 / fromIntegral sample.sampling
          }
  | otherwise = Nothing

-- | Encode a report in the StatsD line protocol.
formatReport :: Report -> ByteString
formatReport report = L.toStrict $ toLazyByteString builder
  where
    builder = byteString report.key <> char8 ':' <> val
    rate
      | report.rate < 1.0 =
          string8 $ printf "|@%f" report.rate
      | otherwise = mempty
    val =
      case report.value of
        Counter i ->
          intDec i <> "|c" <> rate
        Gauge g False ->
          intDec g <> "|g"
        Gauge g True ->
          let sign = if 0 <= g then char8 '+' else mempty
           in sign <> intDec g <> "|g"
        Timing t ->
          intDec t <> "|ms" <> rate
        Set e ->
          byteString e <> "|s"

-- | Send a sample if the sampling rate selects it.
submit :: (MonadIO m) => Stats -> Sample -> m ()
submit stats sample =
  forM_ (toReport stats.params sample) (send stats)

-- | Send a report, ignoring network errors.
--
-- Errors are ignored on purpose: the alternative, failing the code that recorded
-- a metric because the network is down, is never what a program wants.
send :: (MonadIO m) => Stats -> Report -> m ()
send stats report =
  liftIO $
    handleIO (const (return ())) $
      void $
        Net.send
          stats.socket
          (formatReport report <> bool "" "\n" stats.params.newline)

-- | Resolve a host and port and create a UDP socket connected to it. Calls
-- 'error' if the host cannot be resolved.
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

-- | Parse a line in the StatsD protocol. The inverse of 'formatReport'.
parseReport :: (MonadPlus m) => ByteString -> m Report
parseReport bs =
  case C.split '|' bs of
    [kv, t] -> do
      (k, v) <- parseKeyValue kv t
      return $ Report k v 1.0
    [kv, t, r] -> do
      (k, v) <- parseKeyValue kv t
      x <- parseRate r
      return $ Report k v x
    _ -> mzero
  where
    parseKeyValue kv t = do
      case C.split ':' kv of
        [key, v] -> do
          guard (validateKey key)
          value <- parseValue v t
          return (key, value)
        _ -> mzero
    parseValue v t =
      case C.unpack t of
        "c" -> Counter <$> parseNatural v
        "g" ->
          case C.uncons v of
            Just ('+', _) -> Gauge <$> parseInt v <*> pure True
            Just ('-', _) -> Gauge <$> parseInt v <*> pure True
            _ -> Gauge <$> parseNatural v <*> pure False
        "s" -> do
          guard (validateKey v)
          return (Set v)
        "ms" -> Timing <$> parseNatural v
        _ -> mzero
    parseRate r = case C.uncons r of
      Just ('@', s) -> do
        t <- parseRead s
        guard (t > 0.0)
        guard (t < 1.0)
        return t
      _ -> mzero

-- | Parse a value with its 'Read' instance.
parseRead :: (MonadPlus m, Read a) => ByteString -> m a
parseRead = maybe mzero return . readMaybe . C.unpack

-- | Parse a signed integer that takes up the whole string.
parseInt :: (MonadPlus m) => ByteString -> m Int
parseInt bs = case C.readInt bs of
  Just (i, bs') | B.null bs' -> return i
  _ -> mzero

-- | Parse a non-negative integer that takes up the whole string.
parseNatural :: (MonadPlus m) => ByteString -> m Int
parseNatural bs = case C.readInt bs of
  Just (i, bs') | B.null bs' -> guard (0 <= i) >> return i
  _ -> mzero
