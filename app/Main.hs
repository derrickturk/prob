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

data VoiStage = VoiStage { stageCost :: Double
                         , stageChance :: Double
                         }

voiGame :: [VoiStage] -> StateT Double Prob Double
voiGame stages = do
  actualSuccess <- lift $ binary (0.3 :: Double)
  doIt <- runStages actualSuccess stages
  when doIt $ modify (subtract 200)
  when (doIt && actualSuccess) $ do
    payout <- lift $ lognormalMeanRatio 500 5
    modify (+ payout)
  get

runStages :: Bool -> [VoiStage] -> StateT Double Prob Bool
runStages _ [] = return True
runStages actualSuccess (s:stages) = do
  modify (subtract $ stageCost s)
  correctMeasurement <- lift $ binary $ stageChance s
  if correctMeasurement && not actualSuccess
    then return False
    else runStages actualSuccess stages

explStage :: VoiStage
explStage = VoiStage {
                       stageCost = 50
                     , stageChance = 0.6
                     }

appStage :: VoiStage
appStage = VoiStage {
                      stageCost = 50
                    , stageChance = 0.9
                    }

naiveGame :: StateT Double Prob Double
naiveGame = voiGame []

stagedGame :: StateT Double Prob Double
stagedGame = voiGame [explStage, appStage]

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
    withFile file AppendMode $ flip hPutStrLn $ showRNamed "naiveGame" ts
    ts <- sampleProbRIO $ trials 10000 $ evalStateT stagedGame 0.0
    withFile file AppendMode $ flip hPutStrLn $ showRNamed "stagedGame" ts
