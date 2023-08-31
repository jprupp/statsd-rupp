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
    defStatConfig,
  )
where

import Data.HashSet qualified as HashSet
import System.Metrics.StatsD.Internal
  ( Key,
    Sampling,
    StatConfig (..),
    Stats,
    Store (StoreCounter, StoreGauge, StoreSet, StoreTimings),
    Value (Counter, Gauge, Set, Timing),
    newMetric,
    newStats,
    processSample,
    statsLoop,
  )
import UnliftIO (MonadIO, MonadUnliftIO)
import UnliftIO.Async (link, withAsync)

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

defStatConfig :: StatConfig
defStatConfig =
  StatConfig
    { reportStats = True,
      reportSamples = True,
      namespace = "",
      statsPrefix = "stats",
      prefixCounter = "counters",
      prefixTimer = "timers",
      prefixGauge = "gauges",
      prefixSet = "sets",
      server = "127.0.0.1",
      port = 8125,
      flushInterval = 1000,
      timingPercentiles = [90, 95]
    }

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

incrementCounter :: (MonadIO m) => StatCounter -> Int -> m ()
incrementCounter StatCounter {..} =
  processSample stats sampling key . Counter

setGauge :: (MonadIO m) => StatGauge -> Int -> m ()
setGauge StatGauge {..} =
  processSample stats sampling key . Gauge

addTiming :: (MonadIO m) => StatTiming -> Int -> m ()
addTiming StatTiming {..} =
  processSample stats sampling key . Timing

newSetElement :: (MonadIO m) => StatSet -> String -> m ()
newSetElement StatSet {..} =
  processSample stats 1 key . Set

withStats :: (MonadUnliftIO m) => StatConfig -> (Stats -> m a) -> m a
withStats cfg go = do
  stats <- newStats cfg
  if cfg.reportStats
    then withAsync (statsLoop stats) (\a -> link a >> go stats)
    else go stats