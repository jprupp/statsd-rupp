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
    parseReport,
  )
where

import Control.Monad (MonadPlus (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.HashSet qualified as HashSet
import System.Metrics.StatsD.Internal
  ( Key,
    MetricData (..),
    Report (..),
    Sampling,
    StatConfig (..),
    StatCounter (..),
    StatGauge (..),
    StatSet (..),
    StatTiming (..),
    Stats,
    Value (..),
    newMetric,
    newStats,
    processSample,
    statsLoop,
    validateKey,
  )
import Text.Read (readMaybe)
import UnliftIO (MonadIO, MonadUnliftIO)
import UnliftIO.Async (link, withAsync)

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
      timingPercentiles = [90, 95],
      newline = False
    }

newStatCounter ::
  (MonadIO m) => Stats -> Key -> Sampling -> m (Maybe StatCounter)
newStatCounter stats key sampling = do
  success <- newMetric stats key (CounterData 0)
  if success
    then return $ Just $ StatCounter stats key sampling
    else return Nothing

newStatGauge ::
  (MonadIO m) => Stats -> Key -> Int -> Int -> m (Maybe StatGauge)
newStatGauge stats key sampling ini = do
  success <- newMetric stats key (GaugeData ini)
  if success
    then return $ Just $ StatGauge stats key sampling
    else return Nothing

newStatTiming :: (MonadIO m) => Stats -> Key -> Int -> m (Maybe StatTiming)
newStatTiming stats key sampling = do
  success <- newMetric stats key (TimingData [])
  if success
    then return $ Just $ StatTiming stats key sampling
    else return Nothing

newStatSet :: (MonadIO m) => Stats -> Key -> m (Maybe StatSet)
newStatSet stats key = do
  success <- newMetric stats key (SetData HashSet.empty)
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

parseReport :: (MonadPlus m) => ByteString -> m Report
parseReport bs =
  case C.split '|' bs of
    [kv, t] -> do
      (k, v) <- parseKeyValue kv t
      return $ Report k v 1
    [kv, t, r] -> do
      (k, v) <- parseKeyValue kv t
      x <- parseRate r
      return $ Report k v x
    _ -> mzero
  where
    parseRead :: (MonadPlus m, Read a) => String -> m a
    parseRead = maybe mzero return . readMaybe
    parseKeyValue kv t = do
      case C.split ':' kv of
        [k, v] -> do
          key <- parseKey k
          value <- parseValue v t
          return (key, value)
        _ -> mzero
    parseKey k =
      let s = C.unpack k
       in if validateKey s
            then return s
            else mzero
    parseValue v t =
      let s = C.unpack v
       in case t of
            "c" -> Counter <$> parseRead s
            "g" -> Gauge <$> parseRead s
            "s" -> return $ Set s
            "ms" -> Timing <$> parseRead s
            _ -> mzero
    parseRate r = case C.unpack r of
      '@' : s -> parseRead s
      _ -> mzero