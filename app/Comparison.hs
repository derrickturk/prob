{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Prob
import Control.Monad (when, replicateM)
import Data.List (intersperse)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(AppendMode, WriteMode), hPrint, hPutStrLn)

data VoiDevelopment = VoiDevelopment { developmentCost :: Double
                                     , successChance :: Double
                                     , successPayoutMean :: Double
                                     , successPayoutRatio :: Double
                                     }

data VoiStage = VoiStage { stageCostMean :: Double
                         , stageCostSd :: Double
                         , stageChance :: Double
                         }

voiGame :: VoiSuccess -> [VoiStage] -> StateT Double Prob Double
voiGame VoiSuccess{..} stages = do
  actualSuccess <- lift $ binary (0.25 :: Double)
  doIt <- runStages actualSuccess stages
  when doIt $ do
    cost <- lift $ normal 500 50
    modify (subtract cost)
  when (doIt && actualSuccess) $ do
    payout <- lift $ lognormalMeanRatio successPayoutMean successPayoutRatio
    modify (+ payout)
  get

runStages :: Bool -> [VoiStage] -> StateT Double Prob Bool
runStages _ [] = return True
runStages actualSuccess (VoiStage{..}:stages) = do
  do
    cost <- lift $ normal (stageCostMean s) (stageCostSd s)
    modify (subtract cost)
  correctMeasurement <- lift $ binary $ stageChance s
  if correctMeasurement && not actualSuccess
    then return False
    else runStages actualSuccess stages
