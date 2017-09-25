module Main where

import Control.Monad.Prob
import Control.Monad (replicateM)

die :: Prob Int
die = uniform [1..6]

rollSomeDice :: Int -> Prob Int
rollSomeDice = (fmap sum) . flip replicateM die

main :: IO ()
main = putStrLn "hello"
