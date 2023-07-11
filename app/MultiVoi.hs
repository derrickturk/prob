{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Random (setStdGen, mkStdGen)
import System.Environment (getArgs)

import qualified Data.Sequence as S
import Data.List (intercalate, intersperse)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Control.Monad (when, replicateM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Prob

data VoiDevelopment = VoiDevelopment { developmentCostMean :: Double
                                     , developmentCostSd :: Double
                                     , successChance :: Double
                                     , successPayoutMean :: Double
                                     , successPayoutRatio :: Double
                                     }

data VoiStage = VoiStage { stageCostMean :: Double
                         , stageCostSd :: Double
                         , stageChance :: Double
                         }

data PeriodResult = PeriodResult { periodSuccess :: Bool
                                 , periodExpense :: Double
                                 , periodRevenue :: Double
                                 } deriving Show

runProject :: VoiDevelopment -> [VoiStage] -> Prob [PeriodResult]
runProject VoiDevelopment{..} stages = do
  actualSuccess <- binary successChance
  go True actualSuccess stages
  where
    go True actualSuccess [] = do
      cost <- normal developmentCostMean developmentCostSd
      payout <- if actualSuccess
        then lognormalMeanRatio successPayoutMean successPayoutRatio
        else return 0.0
      return [PeriodResult actualSuccess cost payout]

    go False actualSuccess [] = return [PeriodResult (not actualSuccess) 0.0 0.0]

    go True actualSuccess (s:rest) = do
      (continue, result) <- runStage actualSuccess s
      (result:) <$> go continue actualSuccess rest

    go False actualSuccess (_:rest) =
      ((PeriodResult actualSuccess 0.0 0.0):) <$> go False actualSuccess rest

runStage :: Bool -> VoiStage -> Prob (Bool, PeriodResult)
runStage actualSuccess VoiStage{..} = do
  cost <- normal stageCostMean stageCostSd
  correctMeasurement <- binary stageChance
  let goAhead = actualSuccess && correctMeasurement
                || not actualSuccess && not correctMeasurement
  return (goAhead, PeriodResult correctMeasurement cost 0.0) 

voiProject :: Prob (VoiDevelopment, VoiStage, VoiStage)
voiProject = do
  devCostMean <- normal 500.0 100.0
  devCostSd <- normal (devCostMean / 5.0) (devCostMean / 25.0)
  devSuccessChance <- uniform (10.0, 40.0)
  devSuccessPayoutMean <- normal 1500.0 500.0 
  devSuccessPayoutRatio <- uniform (2.5, 7.0)

  explCostMean <- normal 50.0 15.0
  explCostSd <- normal (explCostMean / 5.0) (explCostMean / 25.0)
  explChance <- uniform (0.6, 0.8)

  applCostMean <- normal 100.0 20.0
  applCostSd <- normal (applCostMean / 5.0) (applCostMean / 25.0)
  applChance <- uniform (0.8, 0.95)

  let dev = VoiDevelopment
                           devCostMean
                           devCostSd
                           devSuccessChance
                           devSuccessPayoutMean
                           devSuccessPayoutRatio

  let exp = VoiStage explCostMean explCostSd explChance

  let app = VoiStage applCostMean applCostSd applChance

  return (dev, exp, app)

netCF :: PeriodResult -> Double
netCF PeriodResult{..} = periodRevenue - periodExpense

runStagedPortfolio :: Int -> Int -> Prob [S.Seq [PeriodResult]]
runStagedPortfolio projects trialsEach = replicateM projects $ do
  (dev, expl, appl) <- voiProject
  S.fromList <$> trials trialsEach (runProject dev [expl, appl])

quantile :: (RealFrac b, Fractional b, Ord b) => S.Seq a -> b -> a
quantile seq q =
  let n = fromIntegral $ length seq
      qi = floor $ min (n - 1) (max 0 q * n) in
  S.index seq qi

periodEmpty :: PeriodResult
periodEmpty = PeriodResult False 0.0 0.0

-- note: periodSuccess is not meaningful for EV result
ev :: S.Seq [PeriodResult] -> [PeriodResult]
ev trials = foldl' f [] trials where
  f evs trial = zipWith acc (evs ++ repeat periodEmpty) trial
  acc evResult thisResult =
    PeriodResult
      False
      (periodExpense evResult + (periodExpense thisResult) / n)
      (periodRevenue evResult + (periodRevenue thisResult) / n)
  n = fromIntegral $ S.length trials

netValue :: [PeriodResult] -> Double
netValue = foldl' f 0.0 where
  f net PeriodResult{..} = net + periodRevenue - periodExpense

p50success :: S.Seq [PeriodResult] -> (Maybe [PeriodResult], Double)
p50success trials = if null sorted
  then (Nothing, 0.0)
  else (Just (quantile sorted 0.5),
              fromIntegral (length filtered) / fromIntegral (length trials))
  where
    filtered = S.filter ((>= 0.0) . netValue) trials
    sorted = S.unstableSortBy compare' filtered
    compare' x y = compare (netValue x) (netValue y)

discreteFail :: [PeriodResult] -> [PeriodResult]
discreteFail [] = []
discreteFail [x] = [periodEmpty]
discreteFail (x:xs) = x:(discreteFail xs)

type DrillSchedule = [([PeriodResult], Int)]

scheduleTotalNet :: DrillSchedule -> [Double]
scheduleTotalNet = fmap snd . M.toAscList . foldl' f M.empty where
  f map (results, year) = snd $ foldl' g (year, map) results
  g (year, map) result = (year + 1, M.alter (addNet $ netCF result) year map)
  addNet plus current = Just $ fromMaybe 0 current + plus

scheduleTotalMetrics :: DrillSchedule -> [(Double, Double)]
scheduleTotalMetrics = fmap snd . M.toAscList . foldl' f M.empty where
  f map (results, year) = snd $ foldl' g (year, map) results
  g (year, map) result = (year + 1, M.alter (addResult result) year map)
  addResult PeriodResult{..} current = case current of
    Just (exp, rev) -> Just (exp + periodExpense, rev + periodRevenue)
    Nothing -> Just (periodExpense, periodRevenue)

evSchedule :: [Int] -> [PeriodResult] -> DrillSchedule
evSchedule = go 0 where
  go _ [] _ = []
  go year (n:rest) ev = replicate n (ev, year) ++ go (year + 1) rest ev

discreteSchedule :: [(Int, Int)] -> [PeriodResult] -> DrillSchedule
discreteSchedule = go 0 where
  go _ [] _ = []
  go year ((succ, fail):rest) char =
    replicate succ (char, year) ++
    replicate fail (discreteFail char, year) ++
    go (year + 1) rest char

main :: IO ()
main = do
  args <- getArgs
  when (length args == 1) $ do
    let file = head args

    setStdGen (mkStdGen 12345)
    ts <- sampleProbRIO $ (runStagedPortfolio 100 1000)

    let evs = ev <$> ts
        p50s = p50success <$> ts
        enumerated = zip3 [0..] evs p50s
        nets = zip [0..] $ (fmap . fmap) netValue ts

    withFile file WriteMode $ \h -> do
      hPutStrLn h "Project\tCharacterization\tMetric\t0\t1\t2"
      traverse_ (printProject h) enumerated

    withFile (file ++ "_detail") WriteMode $ \h -> do
      hPutStrLn h "Project\tNet Value"
      traverse_ (printNets h) nets

    withFile (file ++ "_picked") WriteMode $ \h -> do
      hPutStrLn h "Project\tCharacterization\tPeriod\tNet Cashflow\tPs"
      traverse_ (printPicked h) enumerated

    withFile (file ++ "_picked_metrics") WriteMode $ \h -> do
      hPutStrLn h "Project\tCharacterization\tPeriod\tExpense\tRevenue"
      traverse_ (printPickedMetrics h) enumerated

    where
      printProject h (i, ev, (p50, _)) = do
        let p50' = fromMaybe [periodEmpty, periodEmpty, periodEmpty] p50
        hPutStr h (show i)
        hPutStr h "\tEV\tExpense\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodExpense) <$> ev
        hPutStr h (show i)
        hPutStr h "\tEV\tRevenue\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodRevenue) <$> ev
        hPutStr h (show i)
        hPutStr h "\tDiscrete Fail\tExpense\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodExpense) <$> discreteFail p50'
        hPutStr h (show i)
        hPutStr h "\tDiscrete Fail\tRevenue\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodRevenue) <$> discreteFail p50'
        hPutStr h (show i)
        hPutStr h "\tDiscrete Success\tExpense\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodExpense) <$> p50'
        hPutStr h (show i)
        hPutStr h "\tDiscrete Success\tRevenue\t"
        hPutStrLn h $ intercalate "\t" $ (show . periodRevenue) <$> p50'

      printNets h (i, nets) = traverse_
        (\n -> hPutStr h (show i) >> hPutStr h "\t" >> hPutStrLn h (show n))
        nets

      printPicked h (i, ev, (p50, pS)) = do
        let p50' = fromMaybe [periodEmpty, periodEmpty, periodEmpty] p50
            evNet = scheduleTotalNet $
              evSchedule (replicate 8 6) ev
            p50Succs = round (6 * pS)
            p50Fails = 6 - p50Succs
            p50Net = scheduleTotalNet $
              discreteSchedule (replicate 8 (p50Succs, p50Fails)) p50'
        traverse_
          (\(yr, net) -> hPutStrLn h $
            show i ++ "\tEV\t" ++ show yr ++ "\t" ++ show net ++ "\t" ++ show pS)
          (zip [0..] evNet)
        traverse_
          (\(yr, net) -> hPutStrLn h $
            show i ++ "\tDiscrete\t" ++ show yr ++ "\t" ++ show net ++ "\t" ++ show pS)
          (zip [0..] p50Net)

      printPickedMetrics h (i, ev, (p50, pS)) = do
        let p50' = fromMaybe [periodEmpty, periodEmpty, periodEmpty] p50
            evNet = scheduleTotalMetrics $ evSchedule (replicate 8 6) ev
            p50Succs = round (6 * pS)
            p50Fails = 6 - p50Succs
            p50Net = scheduleTotalMetrics $
              discreteSchedule (replicate 8 (p50Succs, p50Fails)) p50'
        traverse_
          (\(yr, (exp, rev)) -> hPutStrLn h $
            show i ++ "\tEV\t" ++ show yr ++ "\t" ++ show exp ++ "\t" ++ show rev)
          (zip [0..] evNet)
        traverse_
          (\(yr, (exp, rev)) -> hPutStrLn h $
            show i ++ "\tDiscrete\t" ++ show yr ++ "\t" ++ show exp ++ "\t" ++ show rev)
          (zip [0..] p50Net)
