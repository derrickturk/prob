{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Prob
import Control.Monad (when, replicateM)
import Data.List (intersperse)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(AppendMode, WriteMode), hPrint, hPutStrLn)

die :: Prob Int
die = uniformDiscrete [1..6]

rollSomeDice :: Int -> Prob Int
rollSomeDice = (fmap sum) . flip replicateM die

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Show, Ord, Enum, Bounded)

data Suit = Hearts
          | Clubs
          | Diamonds
          | Spades
          deriving (Eq, Show, Enum, Bounded)

data Card = Card !Rank !Suit deriving (Eq, Show)

deck :: [Card]
deck = [Card r s | r <- [minBound..maxBound], s <- [minBound..maxBound]]

type CardGame a = State [Card] a

drawCard :: Prob Card
drawCard = do
  rank <- uniformDiscrete [minBound..maxBound]
  suit <- uniformDiscrete [minBound..maxBound]
  return $ Card rank suit

bimodalSkewed :: Prob Double
bimodalSkewed = choose (0.6 :: Double) success failure where
  success = condition (< 1e6) $ lognormalMeanRatio 1.5e5 2
  failure = normal (-2e5) 25000

pokerDecision :: StateT Double Prob Double
pokerDecision = do
  modify (subtract 5) -- ante up
  stayForFlop <- lift $ binary (0.7 :: Double)
  when stayForFlop $ do
    modify (subtract 5) -- 5 more to stay in
    stayForTurn <- lift $ binary (0.5 :: Double)
    when stayForTurn $ do
      modify (subtract 5)
      stayForRiver <- lift $ binary (0.6 :: Double)
      when stayForRiver $ do
        modify (subtract 5)
        winHand <- lift $ binary (0.7 :: Double)
        when winHand $ modify (+ 120) -- we win the pot
  get -- return our final payout or loss

showR :: Show a => [a] -> String
showR = ("c(" ++) . (++ ")") . concat . intersperse ", " . fmap show

showRNamed :: Show a => String -> [a] -> String
showRNamed name = (name ++) . (" <- " ++) . showR

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

voiGame :: VoiDevelopment -> [VoiStage] -> StateT Double Prob (Double, Bool)
voiGame VoiDevelopment{..} stages = do
  actualSuccess <- lift $ binary successChance
  doIt <- runStages actualSuccess stages
  when doIt $ do
    cost <- lift $ normal developmentCostMean developmentCostSd
    modify (subtract cost)
  when (doIt && actualSuccess) $ do
    payout <- lift $ lognormalMeanRatio successPayoutMean successPayoutRatio
    modify (+ payout)
  net <- get
  return (net, doIt == actualSuccess)

runStages :: Bool -> [VoiStage] -> StateT Double Prob Bool
runStages _ [] = return True
runStages actualSuccess (s:stages) = do
  do
    cost <- lift $ normal (stageCostMean s) (stageCostSd s)
    modify (subtract cost)
  correctMeasurement <- lift $ binary $ stageChance s
  if correctMeasurement && not actualSuccess
     || not correctMeasurement && actualSuccess
    then return False
    else runStages actualSuccess stages

development :: VoiDevelopment
development = VoiDevelopment { developmentCostMean = 500
                             , developmentCostSd = 100
                             , successChance = 0.25
                             , successPayoutMean = 1500.0
                             , successPayoutRatio = 5.0
                             }

explStage :: VoiStage
explStage = VoiStage { stageCostMean = 50
                     , stageCostSd = 10
                     , stageChance = 0.70
                     }

appStage :: VoiStage
appStage = VoiStage { stageCostMean = 100
                    , stageCostSd = 20
                    , stageChance = 0.90
                    }

naiveGame :: StateT Double Prob (Double, Bool)
naiveGame = voiGame development []

stagedGame :: StateT Double Prob (Double, Bool)
stagedGame = voiGame development [explStage, appStage]

main :: IO ()
main = do
  args <- getArgs
  when (length args == 1) $ do
    let file = head args
    ts <- sampleProbRIO $ trials 1000 bimodalSkewed
    withFile file WriteMode $ flip hPutStrLn $ "bimodal <- " ++ showR ts
    ts <- sampleProbRIO $ trials 10000 $ evalStateT pokerDecision 0.0
    withFile file AppendMode $ flip hPutStrLn $ showRNamed "poker" ts

    ts <- sampleProbRIO $ trials 10000 $ evalStateT naiveGame 0.0
    withFile file AppendMode $
      flip hPutStrLn $ showRNamed "naiveGame" (fst <$> ts)
    putStr "naive chance of correct decision: "
    print $ (fromIntegral $ length (filter snd ts)) / 10000.0

    ts <- sampleProbRIO $ trials 10000 $ evalStateT stagedGame 0.0
    withFile file AppendMode $
      flip hPutStrLn $ showRNamed "stagedGame" (fst <$> ts)
    putStr "staged chance of correct decision: "
    print $ (fromIntegral $ length (filter snd ts)) / 10000.0
