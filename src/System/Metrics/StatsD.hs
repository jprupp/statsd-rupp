{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : System.Metrics.StatsD
-- Copyright   : (c) 2023 JP Rupp
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- A <https://github.com/statsd/statsd StatsD> client.
--
-- This library can work in two ways at the same time:
--
-- * Like a simple StatsD client, sending each sample to the StatsD server as it
--   happens. Enable this with 'reportSamples'.
--
-- * Like a StatsD server, aggregating samples in memory and sending statistics
--   such as counts, rates, means and percentiles every 'flushInterval'
--   milliseconds. Enable this with 'reportStats'. This mode is useful when
--   sending to a backend that accepts the StatsD protocol but does not
--   aggregate, such as a Graphite or InfluxDB relay.
--
-- All metrics are sent over UDP. Network errors are silently ignored, so that
-- metrics never break the application.
--
-- = Purpose
--
-- Metrics tell you what a running program is doing: how many requests it
-- serves, how long they take, how full its queues are. StatsD is a simple and
-- widely supported way to ship them: one line of text per sample, sent over
-- UDP to a daemon that aggregates and forwards them.
--
-- The aggregation mode exists because not every collector aggregates. Some
-- accept the StatsD line protocol but store whatever they receive, so a busy
-- service sending one line per request floods them. Aggregating in the process
-- and sending a fixed set of statistics once per interval keeps the traffic
-- constant however busy the program is, and puts percentiles in reach of
-- backends that cannot compute them.
--
-- = Design
--
-- * __Metrics must never hurt the program.__ Sending is fire-and-forget over
--   UDP and send errors are ignored. A missing or overloaded StatsD server
--   costs some lost samples, never a failed request.
--
-- * __Mistakes in naming fail early.__ Invalid keys, duplicate keys and invalid
--   configurations are refused when the metric or client is created, not when
--   the first sample is sent. A typo shows up at start-up instead of as a
--   missing graph weeks later.
--
-- * __Register first, then update cheaply.__ Each metric is created once and
--   returns a handle. Updating it is an 'STM' update of a map and, when samples
--   are on, one UDP packet, so it is cheap enough for hot paths.
--
-- * __Sampling saves traffic, not accuracy.__ Sampling only thins out the
--   individual samples sent. The aggregated statistics always count every
--   reading.
--
-- = Example
--
-- @
-- main :: IO ()
-- main =
--     'withStats' 'defStatConfig'{'namespace' = "myapp"} $ \\stats -> do
--         requests <- 'newStatCounter' stats "requests" 1
--         latency  <- 'newStatTiming' stats "latency" 10
--         users    <- 'newStatGauge' stats "users" 0
--         visitors <- 'newStatSet' stats "visitors"
--         'incrementCounter' requests 1
--         'addTiming' latency 42
--         'incrementGauge' users 1
--         'newSetElement' visitors "alice"
-- @
--
-- = Metric names
--
-- Metric keys may only contain ASCII letters, digits, @.@, @_@ and @-@. Each key
-- can only be registered once per 'Stats' instance. With the default
-- configuration and a namespace of @myapp@, the following names are sent:
--
-- [Samples] @myapp.\<key\>@
--
-- [Counters] @myapp.stats.counters.\<key\>.count@ and
--   @myapp.stats.counters.\<key\>.rate@ (per second)
--
-- [Gauges] @myapp.stats.gauges.\<key\>@
--
-- [Sets] @myapp.stats.sets.\<key\>.count@ (distinct elements seen during the
--   interval)
--
-- [Timings] @myapp.stats.timers.\<key\>.\<stat\>@, where @\<stat\>@ is one of
--   @count@, @count_ps@, @std@, @mean@, @upper@, @lower@, @sum@, @sum_squares@
--   and @median@. For each configured percentile @N@, the same statistics except
--   @count_ps@ and @std@ are also sent with an @_N@ suffix, computed over the
--   lowest @N@% of the timings.
module System.Metrics.StatsD
  ( -- * Configuration
    StatConfig (..),
    defStatConfig,
    -- * Running
    Stats,
    withStats,
    -- * Counters
    StatCounter,
    newStatCounter,
    incrementCounter,
    -- * Gauges
    StatGauge,
    newStatGauge,
    setGauge,
    incrementGauge,
    decrementGauge,
    -- * Timings
    StatTiming,
    newStatTiming,
    addTiming,
    -- * Sets
    StatSet,
    newStatSet,
    newSetElement,
  )
where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as C
import Data.HashSet qualified as HashSet
import System.Metrics.StatsD.Internal
  ( MetricData (..),
    StatConfig (..),
    StatCounter (..),
    StatGauge (..),
    StatSet (..),
    StatTiming (..),
    Stats,
    Value (..),
    connectStatsD,
    newMetric,
    newMetrics,
    newStats,
    processSample,
    statsLoop,
  )
import UnliftIO (MonadIO, MonadUnliftIO, throwIO)
import UnliftIO.Async (link, withAsync)

-- | Name of a metric. Only ASCII letters, digits, @.@, @_@ and @-@ are allowed.
type Key = String

-- | Default configuration:
--
-- * Report both aggregated statistics and individual samples.
-- * No namespace.
-- * Prefixes @stats@, @counters@, @timers@, @gauges@ and @sets@.
-- * Send to @127.0.0.1@ port @8125@.
-- * Flush statistics every second.
-- * Compute the 90th and 95th percentiles of timings.
-- * Do not append a newline to each line sent.
defStatConfig :: StatConfig
defStatConfig =
  StatConfig
    { reportStats = True,
      reportSamples = True,
      namespace = "",
      prefixStats = "stats",
      prefixCounter = "counters",
      prefixTimer = "timers",
      prefixGauge = "gauges",
      prefixSet = "sets",
      statsdServer = "127.0.0.1",
      statsdPort = 8125,
      flushInterval = 1000,
      timingPercentiles = [90, 95],
      appendNewline = False
    }

-- | Register a new counter.
--
-- The last argument is the sampling rate for individual samples sent when
-- 'reportSamples' is on: one in every @n@ increments is sent, tagged with a rate
-- of @1\/n@ so that the StatsD server can scale it back up. Use @1@ to send
-- every increment and @0@ to send none. The aggregated statistics always count
-- every increment.
--
-- Throws an 'IOError' if the sampling rate is negative, if the key is invalid,
-- or if a metric with the same key already exists.
--
-- Sampling is per metric because traffic is: a counter incremented on every
-- request needs a low rate, while one incremented on every error can afford to
-- send everything.
newStatCounter ::
  (MonadIO m) => Stats -> Key -> Int -> m StatCounter
newStatCounter stats key sampling = do
  when (sampling < 0) $
    throwIO $
      userError "Counter sampling rate should not be negative"
  newMetric stats (C.pack key) (CounterData 0)
  return $ StatCounter stats (C.pack key) sampling

-- | Register a new gauge with an initial value.
--
-- Throws an 'IOError' if the key is invalid or a metric with the same key
-- already exists.
newStatGauge ::
  (MonadIO m) => Stats -> Key -> Int -> m StatGauge
newStatGauge stats key ini = do
  newMetric stats (C.pack key) (GaugeData ini)
  return $ StatGauge stats (C.pack key)

-- | Register a new timing metric. The last argument is the sampling rate for
-- individual samples, with the same meaning as in 'newStatCounter'.
--
-- Throws an 'IOError' if the sampling rate is negative, if the key is invalid,
-- or if a metric with the same key already exists.
newStatTiming :: (MonadIO m) => Stats -> Key -> Int -> m StatTiming
newStatTiming stats key sampling = do
  when (sampling < 0) $
    throwIO $
      userError "Timing sampling rate should not be negative"
  newMetric stats (C.pack key) (TimingData [])
  return $ StatTiming stats (C.pack key) sampling

-- | Register a new set, which counts the distinct elements seen during each
-- flush interval.
--
-- Throws an 'IOError' if the key is invalid or a metric with the same key
-- already exists.
newStatSet :: (MonadIO m) => Stats -> Key -> m StatSet
newStatSet stats key = do
  newMetric stats (C.pack key) (SetData HashSet.empty)
  return $ StatSet stats (C.pack key)

-- | Add a number to a counter. The aggregated count is reset to zero after each
-- flush.
incrementCounter :: (MonadIO m) => StatCounter -> Int -> m ()
incrementCounter StatCounter {..} =
  processSample stats sampling key . Counter

-- | Set a gauge to a value. Gauges keep their value across flushes.
--
-- Note that in the StatsD protocol a negative value is interpreted as a
-- decrement, so StatsD servers receiving samples will not set the gauge to a
-- negative value.
setGauge :: (MonadIO m) => StatGauge -> Int -> m ()
setGauge StatGauge {..} i =
  processSample stats 1 key (Gauge i False)

-- | Add a number to a gauge, which may be negative. The aggregated value never
-- goes below zero and saturates at 'maxBound'.
incrementGauge :: (MonadIO m) => StatGauge -> Int -> m ()
incrementGauge StatGauge {..} i =
  processSample stats 1 key (Gauge i True)

-- | Subtract a number from a gauge. The aggregated value never goes below zero.
decrementGauge :: (MonadIO m) => StatGauge -> Int -> m ()
decrementGauge x i = incrementGauge x (negate i)

-- | Record a timing, normally a duration in milliseconds. The timings collected
-- during a flush interval are summarised and then discarded.
addTiming :: (MonadIO m) => StatTiming -> Int -> m ()
addTiming StatTiming {..} =
  processSample stats sampling key . Timing

-- | Record that an element was seen in a set. Elements should follow the same
-- character rules as keys, or StatsD servers may reject the sample.
newSetElement :: (MonadIO m) => StatSet -> String -> m ()
newSetElement StatSet {..} =
  processSample stats 1 key . Set . C.pack

-- | Connect to the StatsD server and pass a 'Stats' handle to the given function.
--
-- If 'reportStats' is on, a background thread is started that flushes
-- aggregated statistics every 'flushInterval' milliseconds. It is stopped when
-- the function returns.
--
-- Calls 'error' if the configuration is invalid: if a prefix or the namespace is
-- not a valid key, if 'flushInterval' is not positive, or if a percentile is not
-- strictly between 0 and 100. Also fails if the server address cannot be
-- resolved.
--
-- A scope rather than a global client keeps the background flusher's lifetime
-- explicit, and makes it possible to run several independent clients in one
-- program, for example in tests.
withStats :: (MonadUnliftIO m) => StatConfig -> (Stats -> m a) -> m a
withStats cfg go = do
  metrics <- newMetrics
  socket <- connectStatsD cfg.statsdServer cfg.statsdPort
  let stats = newStats cfg metrics socket
  if cfg.reportStats
    then withAsync (statsLoop stats) (\a -> link a >> go stats)
    else go stats