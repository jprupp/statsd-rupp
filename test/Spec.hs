{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString.Char8 qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString
import System.Metrics.StatsD
import System.Metrics.StatsD.Internal
import Test.Hspec
import UnliftIO

testConfig :: StatConfig
testConfig = defStatConfig {flushInterval = 50}

main :: IO ()
main = hspec $ around (withMockStats testConfig) $ do
  describe "counter" $ do
    it "creates two counters" $ \(stats, _) -> do
      _ <- newStatCounter stats "planets" 1
      _ <- newStatCounter stats "moons" 1
      return () :: Expectation
    it "reports on two counters" $ \(stats, report) -> do
      planets <- newStatCounter stats "planets" 1
      moons <- newStatCounter stats "moons" 1
      incrementCounter planets 10
      incrementCounter moons 20
      rs <- replicateM 2 report
      let rs' =
            [ Report "planets" (Counter 10) 1.0,
              Report "moons" (Counter 20) 1.0
            ]
      rs `shouldMatchList` rs'
    it "filters reported samples" $ \(stats, report) -> do
      c <- newStatCounter stats "moons" 4
      let counts = [11 .. 30]
      forM_ counts (incrementCounter c)
      rs <- replicateM 5 report
      let rs' =
            [ Report "moons" (Counter x) 0.25
              | (i, x) <- zip [(1 :: Int) ..] counts,
                i `mod` 4 == 0
            ]
      rs `shouldMatchList` rs'
      r <- report
      r.key `shouldNotBe` "moons"
    it "reports stats" $ \(stats, report) -> do
      c <- newStatCounter stats "planets" 4
      replicateM_ 20 (incrementCounter c 10)
      replicateM_ 5 report
      do
        rs <- replicateM 2 report
        let rs' =
              [ Report "stats.counters.planets.count" (Counter 200) 1.0,
                Report "stats.counters.planets.rate" (Counter 4000) 1.0
              ]
        rs `shouldMatchList` rs'
      do
        rs <- replicateM 2 report
        let rs' =
              [ Report "stats.counters.planets.count" (Counter 0) 1.0,
                Report "stats.counters.planets.rate" (Counter 0) 1.0
              ]
        rs `shouldMatchList` rs'
  describe "timing" $ do
    it "reports events" $ \(stats, report) -> do
      let timings = [51 .. 55]
      t <- newStatTiming stats "trips" 1
      forM_ timings (addTiming t)
      rs <- replicateM (length timings) report
      let rs' = map (\n -> Report "trips" (Timing n) 1.0) timings
      rs `shouldMatchList` rs'
    it "reports empty stats" $ \(stats, report) -> do
      _ <- newStatTiming stats "holes" 1
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
    it "filters reported samples" $ \(stats, report) -> do
      let timings = [1001 .. 2000]
      t <- newStatTiming stats "cats" 100
      forM_ timings (addTiming t)
      rs <- replicateM 10 report
      let rs' =
            [ Report "cats" (Timing x) 0.01
              | (i, x) <- zip [(1 :: Int) ..] timings,
                i `mod` 100 == 0
            ]
      rs `shouldMatchList` rs'
    it "computes stats" $ const $ do
      let ps = reverse [5 .. 800] ++ [801 .. 1500]
          p90 = take (length ps * 90 `div` 100) (sort ps)
          p95 = take (length ps * 95 `div` 100) (sort ps)
          mean' ls = sum ls `div` length ls
          sumsq = sum . map (\x -> x * x)
          stdev' ls =
            let l = fromIntegral (length ls)
                ds = map (subtract (mean' ls)) ls
                s = fromIntegral $ sumsq ds :: Double
                v = s / l
             in round (sqrt v)
          median' ls
            | even (length ls) =
                let x = length ls `div` 2
                    y = x - 1
                 in (sort ls !! x + sort ls !! y) `div` 2
            | otherwise = sort ls !! (length ls `div` 2)
          ts = makeTimingStats ps
          t90 = trimPercentile 90 ts
          t95 = trimPercentile 95 ts
      length ts.timings `shouldBe` length ps
      mean ts `shouldBe` mean' ps
      stdev ts `shouldBe` stdev' ps
      last ts.cumsquares `shouldBe` sumsq ps
      last ts.cumsums `shouldBe` sum ps
      median ts `shouldBe` median' ps
      length t90.timings `shouldBe` length p90
      mean t90 `shouldBe` mean' p90
      stdev t90 `shouldBe` stdev' p90
      last t90.cumsquares `shouldBe` sumsq p90
      last t90.cumsums `shouldBe` sum p90
      median t90 `shouldBe` median' p90
      length t95.timings `shouldBe` length p95
      mean t95 `shouldBe` mean' p95
      stdev t95 `shouldBe` stdev' p95
      last t95.cumsquares `shouldBe` sumsq p95
      last t95.cumsums `shouldBe` sum p95
      median t95 `shouldBe` median' p95
    it "reports stats" $ \(stats, report) -> do
      let timings = [5 .. 1500]
      t <- newStatTiming stats "kittens" 0
      forM_ timings (addTiming t)
      let l = length timings
          r k v = Report ("stats.timers.kittens." <> k) v 1.0
      do
        let rs' =
              [ r "count" (Counter l),
                r "count_ps" (Counter (l * 20)),
                r "std" (Timing 432),
                r "mean" (Timing 752),
                r "lower" (Timing 5),
                r "upper" (Timing 1500),
                r "sum" (Timing 1125740),
                r "sum_squares" (Timing 1126125220),
                r "median" (Timing 752),
                r "count_95" (Counter 1421),
                r "mean_95" (Timing 715),
                r "lower_95" (Timing 5),
                r "upper_95" (Timing 1425),
                r "sum_95" (Timing 1016015),
                r "sum_squares_95" (Timing 965562395),
                r "median_95" (Timing 715),
                r "count_90" (Counter 1346),
                r "mean_90" (Timing 677),
                r "lower_90" (Timing 5),
                r "upper_90" (Timing 1350),
                r "sum_90" (Timing 911915),
                r "sum_squares_90" (Timing 821036445),
                r "median_90" (Timing 677)
              ]
        rs <- replicateM (length rs') report
        rs `shouldMatchList` rs'
      do
        let rs' =
              concatMap
                (replicate 2)
                [ r "count" (Counter 0),
                  r "count_ps" (Counter 0),
                  r "count_90" (Counter 0),
                  r "count_95" (Counter 0)
                ]
        rs <- replicateM (length rs') report
        rs `shouldMatchList` rs'
  describe "gauge" $ do
    it "reports set events" $ \(stats, report) -> do
      let gauges = [51 .. 55]
      g <- newStatGauge stats "speed" 50
      forM_ gauges (setGauge g)
      rs <- replicateM (length gauges) report
      let rs' = map (\n -> Report "speed" (Gauge n False) 1.0) gauges
      rs `shouldMatchList` rs'
    it "reports stats" $ \(stats, report) -> do
      let gauges = [51 .. 55]
      g <- newStatGauge stats "radius" 50
      r <- report
      r `shouldBe` Report "stats.gauges.radius" (Gauge 50 False) 1.0
      forM_ gauges (setGauge g)
      replicateM_ (length gauges) report
      rs <- replicateM 2 report
      let rs' = replicate 2 $ Report "stats.gauges.radius" (Gauge 55 False) 1.0
      rs `shouldMatchList` rs'
    it "increments and decrements value" $ \(stats, report) -> do
      g <- newStatGauge stats "breadth" 50
      incrementGauge g 5
      report `shouldReturn` Report "breadth" (Gauge 5 True) 1.0
      report `shouldReturn` Report "stats.gauges.breadth" (Gauge 55 False) 1.0
      incrementGauge g (-10)
      report `shouldReturn` Report "breadth" (Gauge (-10) True) 1.0
      report `shouldReturn` Report "stats.gauges.breadth" (Gauge 45 False) 1.0
      decrementGauge g 50
      report `shouldReturn` Report "breadth" (Gauge (-50) True) 1.0
      report `shouldReturn` Report "stats.gauges.breadth" (Gauge 0 False) 1.0
      decrementGauge g (-25)
      report `shouldReturn` Report "breadth" (Gauge 25 True) 1.0
      report `shouldReturn` Report "stats.gauges.breadth" (Gauge 25 False) 1.0
  describe "set" $ do
    it "reports events" $ \(stats, report) -> do
      let set = ["one", "two", "three", "three"]
      s <- newStatSet stats "potatoes"
      forM_ set (newSetElement s)
      rs <- replicateM (length set) report
      let rs' = map (\x -> Report "potatoes" (Set x) 1.0) set
      rs `shouldMatchList` rs'
    it "reports stats" $ \(stats, report) -> do
      let set = ["two", "three", "one", "two", "three", "three"]
      s <- newStatSet stats "bananas"
      forM_ set (newSetElement s)
      let rs' =
            [ Report "stats.sets.bananas.count" (Counter 3) 1.0,
              Report "stats.sets.bananas.count" (Counter 0) 1.0
            ]
      rs <- replicateM (length set + length rs') report
      rs `shouldEndWith` rs'

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
      let rs = map (fromJust . parseReport) bs
      mapM_ (atomically . writeTQueue q) rs

withMockSockets :: (MonadUnliftIO m) => ((Socket, Socket) -> m a) -> m a
withMockSockets =
  bracket
    (liftIO (socketPair AF_UNIX Stream 0))
    (\(s1, s2) -> liftIO (close s1 >> close s2))