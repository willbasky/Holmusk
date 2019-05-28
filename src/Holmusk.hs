module Holmusk
       ( task1
       , task2
       , task3
       ) where

import Control.Monad.MC
import Control.Monad.Primitive (PrimMonad)
import Data.Bool (bool)
import Data.List (minimumBy)
import qualified Data.Summary.Double as SD


-- Constants
redL :: Double
redL = 2

redB :: Double
redB = 1

yellowL :: Double
yellowL = 5

yellowB :: Double
yellowB = 5 / 2

blueL :: Double
blueL = 1

blueB :: Double
blueB =  1 / 5

--- New custom arrives probability from 0 to 1.
arriveProbability :: Double -> Double
arriveProbability t = 1 - exp (time / 100)
  where
    time = bool (negate t) t (t < 0)

-- Time of waiting during teller executing a task.
serviceDistributionTime :: Double -> Double -> Double -> Double
serviceDistributionTime r l b = 200 * (r ** (l - 1)) * ((1 - r) ** (b - 1))

leaveGen :: (PrimMonad m) => Double -> Double -> MC m Double
leaveGen l b = do
  rand <- uniform 0 1
  let w = serviceDistributionTime rand l b
  return w

leaveList :: Double -> Double -> Int -> [Double]
leaveList l b n = replicateMC n (leaveGen l b) (mt19937 0)

arrive :: (PrimMonad m) => MC m Double
arrive = do
  time <- uniform 0 100
  let a = arriveProbability time
  return a

arriveList :: Int -> [Double]
arriveList n = replicateMC n arrive (mt19937 0)

task1 :: Int -> IO ()
task1 n = do
  let cs = leaveList yellowL yellowB n
  let summary = SD.fromList cs
  let maxWait = show $ SD.maximum summary
  let meanWait = show $ SD.mean summary
  putStrLn $ "Yellow maximum waiting time: " ++ maxWait
  putStrLn $ "Yellow mean waiting time: " ++ meanWait

task2 :: Int -> IO ()
task2 n = do
  let cs = leaveList redL redB n
  let summaryCustomer = SD.fromList cs
  let as = arriveList n
  let summaryArrive = SD.fromList as
  let meanArrive = SD.mean summaryArrive
  let maxArrive = SD.maximum summaryArrive
  -- Payload
  let maxIntensity = meanArrive / SD.maximum summaryCustomer
  let meanIntensity = meanArrive / SD.mean summaryCustomer
  -- Queue
  let queueMax = maxIntensity ^ (2 :: Int) / (1 - maxIntensity)
  let queueMean = meanIntensity ^ (2 :: Int) / (1 - meanIntensity)
  putStrLn $ "Red maximum queue: " ++ show queueMax
  putStrLn $ "Red mean queue: " ++ show queueMean
  -- putStrLn $ "Red maxIntensity: " ++ show maxIntensity
  -- putStrLn $ "Red meanIntensity: " ++ show meanIntensity

task3 :: Int -> IO ()
task3 n = do
  let cys = leaveList yellowL yellowB n
  let crs = leaveList redL redB n
  let cbs = leaveList blueL blueB n
  let summaryY = SD.fromList cys
  let summaryR = SD.fromList crs
  let summaryB = SD.fromList cbs
  let maxWaitY = SD.maximum summaryY
  let meanWaitY = SD.mean summaryY
  let maxWaitR = SD.maximum summaryR
  let meanWaitR = SD.mean summaryR
  let maxWaitB = SD.maximum summaryB
  let meanWaitB = SD.mean summaryB
  let closestY = (maxWaitY - meanWaitY) / (maxWaitY + meanWaitY)
  let closestR = (maxWaitR - meanWaitR) / (maxWaitR + meanWaitR)
  let closestB = (maxWaitB - meanWaitB) / (maxWaitB + meanWaitB)
  let (model, value) = minimumBy (\(_,x) (_,y) -> compare x y) $ zip ["yellow", "red", "blue"] [closestY, closestR, closestB]
  putStrLn $ "Closest value is " ++ show value ++ " in " ++ model ++ " model."

-- Task 1
-- Given only yellow customers, what are the average and maximum customer waiting times?

-- Task 2
-- Given only red customers, what are the average and maximum queue lengths in-front of the teller?

-- Task 3
-- Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?
