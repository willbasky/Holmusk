module Main (main) where

import Holmusk


main :: IO ()
main = do
  putStrLn "\nTask 1. Given only yellow customers, what are the average and maximum customer waiting times?"
  task1 10000
  putStrLn "\nTask 2. Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
  task2 10000
  putStrLn "\nTask 3. Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?"
  task3 10000
