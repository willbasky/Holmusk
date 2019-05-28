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
-- import System.Random


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

-- Time of custom waiting in queue.
waitingTime :: Double -> Double -> Double -> Double
waitingTime x l b = 200 * x ** (l - 1) * (1 - x) ** (b - 1)

-- main0 :: IO ()
-- main0 = do
--   time <- getStdRandom $ randomR (1,100)
--   let x =  arriveProbability time
--   let yellow = show $ waitingTime x yellowL yellowB
--   let red = show $ waitingTime x redL redB
--   let blue = show $ waitingTime x blueL blueB
--   mapM_ putStrLn ["yellow: " ++ yellow, "red: " ++ red, "blue: " ++ blue]

customer :: (PrimMonad m) => Double -> Double -> MC m Double
customer l b = do
  time <- uniform 0 100
  let x = arriveProbability time
  let c = waitingTime x l b
  return c

customerList :: Double -> Double -> Seed -> Int -> [Double]
customerList l b seed  n = replicateMC n (customer l b) (mt19937 seed)

task1 :: Int -> IO ()
task1 n = do
  let cys = customerList yellowL yellowB 0 n
  let summary = SD.fromList cys
  let maxWait = show $ SD.maximum summary
  let meanWait = show $ SD.mean summary
  putStrLn $ "Yellow maximum waiting time: " ++ maxWait
  putStrLn $ "Yellow mean waiting time: " ++ meanWait

-- Not sure about it.
task2 :: Int -> IO ()
task2 n = do
  let cys = customerList redL redB 0 n
  let summary = SD.fromList cys
  let minWait = SD.minimum summary
  let maxWait = SD.maximum summary
  let meanWait = SD.mean summary
  let meanQ = show $ lengthQueue meanWait minWait
  let maxQ = show $ lengthQueue maxWait minWait
  -- print summary
  -- putStrLn $ "Red maximum waiting time:: " ++ show maxWait
  -- putStrLn $ "Red mean waiting time:: " ++ show meanWait
  putStrLn $ "Red maximum queue: " ++ maxQ
  putStrLn $ "Red mean queue: " ++ meanQ

lengthQueue :: Double -> Double -> Int
lengthQueue maxWait minWait = length $ go maxWait minWait []
  where
    go maxWait' minWait' list
      | minWait' > maxWait' = list
      | otherwise = go maxWait' (minWait' + minWait) (minWait':list)

task3 :: Int -> IO ()
task3 n = do
  let cys = customerList yellowL yellowB 0 n
  let crs = customerList redL redB 0 n
  let cbs = customerList blueL blueB 0 n
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
