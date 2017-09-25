module Main where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Prob
import Control.Monad (replicateM)

die :: Prob Int
die = uniform [1..6]

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
  rank <- uniform [minBound..maxBound]
  suit <- uniform [minBound..maxBound]
  return $ Card rank suit

main :: IO ()
main = putStrLn "hello"
