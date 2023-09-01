{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString.Char8 qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (isJust, mapMaybe)
import Network.Socket
import Network.Socket.ByteString
import System.Metrics.StatsD
import System.Metrics.StatsD.Internal
import Test.Hspec
import Text.Printf
import UnliftIO

testConfig :: StatConfig
testConfig = defStatConfig {flushInterval = 50}

main :: IO ()
main = hspec $ around (withMockStats testConfig) $ do
  describe "counter" $ do
    it "creates two counters" $ \(stats, _) -> do
      Just _ <- newStatCounter stats "planets" 1
      Just _ <- newStatCounter stats "moons" 1
      return () :: Expectation
    it "reports two counters" $ \(stats, report) -> do
      Just planets <- newStatCounter stats "planets" 1
      Just moons <- newStatCounter stats "moons" 1
      incrementCounter planets 10
      incrementCounter moons 20
      let test r = r.key `elem` ["planets", "moons"]
      rs <- replicateM 2 report
      let rs' =
            [ Report "planets" (Counter 10) 1.0,
              Report "moons" (Counter 20) 1.0
            ]
      rs `shouldMatchList` rs'
    it "filters samples" $ \(stats, report) -> do
      Just c <- newStatCounter stats "moons" 4
      replicateM_ 20 (incrementCounter c 10)
      rs <- replicateM 5 report
      rs `shouldMatchList` replicate 5 (Report "moons" (Counter 10) 0.25)
      r <- report
      r.key `shouldNotBe` "moons"
    it "reports stats" $ \(stats, report) -> do
      Just c <- newStatCounter stats "planets" 4
      replicateM_ 20 (incrementCounter c 10)
      replicateM_ 5 report
      rs <- replicateM 2 report
      let rs' =
            [ Report "stats.counters.planets.count" (Counter 200) 1.0,
              Report "stats.counters.planets.rate" (Counter 4000) 1.0
            ]
      rs `shouldMatchList` rs'
    it "resets stats" $ \(stats, report) -> do
      Just c <- newStatCounter stats "planets" 4
      replicateM_ 20 (incrementCounter c 10)
      replicateM_ 5 report
      replicateM_ 2 report
      rs <- replicateM 2 report
      let rs' =
            [ Report "stats.counters.planets.count" (Counter 0) 1.0,
              Report "stats.counters.planets.rate" (Counter 0) 1.0
            ]
      rs `shouldMatchList` rs'
  describe "timing" $ do
    it "reports timing events" $ \(stats, report) -> do
      let timings = [51 .. 55]
      Just t <- newStatTiming stats "trips" 1
      forM_ timings (addTiming t)
      rs <- replicateM (length timings) report
      let rs' = map (\n -> Report "trips" (Timing n) 1.0) timings
      rs `shouldMatchList` rs'
    it "reports empty timer stats" $ \(stats, report) -> do
      Just _ <- newStatTiming stats "holes" 1
      rs <- replicateM 8 report
      let rs' =
            concatMap
              (replicate 2)
              [ Report "stats.timers.holes.count" (Counter 0) 1.0,
                Report "stats.timers.holes.count_ps" (Counter 0) 1.0,
                Report "stats.timers.holes.count_90" (Counter 0) 1.0,
                Report "stats.timers.holes.count_95" (Counter 0) 1.0
              ]
      rs `shouldMatchList` rs'

filterReports :: (Monad m) => (Report -> Bool) -> m Report -> m Report
filterReports test report = do
  r <- report
  if test r
    then return r
    else filterReports test report

withMockStats ::
  (MonadUnliftIO m) =>
  StatConfig ->
  ((Stats, m Report) -> m a) ->
  m a
withMockStats cfg go =
  withMockSockets $ \(s1, s2) -> do
    q <- newTQueueIO
    withAsync (fwd s2 q) $ \a1 -> do
      link a1
      m <- newTVarIO HashMap.empty
      let s = Stats m cfg {newline = True} s1
      withAsync (statsLoop s) $ \a2 -> do
        link a2
        go (s, atomically (readTQueue q))
  where
    fwd s2 q = forever $ do
      bs <- liftIO $ B.lines <$> recv s2 (2 ^ (20 :: Int))
      let rs = mapMaybe parseReport bs
      mapM_ (atomically . writeTQueue q) rs

withMockSockets :: (MonadUnliftIO m) => ((Socket, Socket) -> m a) -> m a
withMockSockets =
  bracket
    (liftIO (socketPair AF_UNIX Stream 0))
    (\(s1, s2) -> liftIO (close s1 >> close s2))