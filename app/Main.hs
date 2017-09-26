module Main where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Prob
import Control.Monad (when, replicateM)
import Data.List (intersperse)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(WriteMode), hPrint, hPutStrLn)

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
  success = condition (< 1e6) $ exp <$> normal (log 1.5e5) 1.5
  failure = normal (-2e5) 25000

showR :: Show a => [a] -> String
showR = ("c(" ++) . (++ ")") . concat . intersperse ", " . fmap show

main :: IO ()
main = do
  args <- getArgs
  when (length args == 1) $ do
    let file = head args
    trials <- sampleProbRIO $ trials 1000 bimodalSkewed
    withFile file WriteMode $ flip hPutStrLn $ "bimodal <- " ++ showR trials
