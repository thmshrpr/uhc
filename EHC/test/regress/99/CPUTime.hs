{- ----------------------------------------------------------------------------------------
   what    : Testing CPUTime
   expected: ok
   [###] This should not be in regression testing as it does not have stable results
---------------------------------------------------------------------------------------- -}

module Main where

import System.CPUTime

main :: IO ()

main = do
  putStrLn $ show cpuTimePrecision
  time <- getCPUTime
  putStrLn $ show time
  putStrLn $ show $ f ([1..100000] :: [Int])
  time' <- getCPUTime
  putStrLn $ show time'

f :: [Int] -> Int
f xs = length xs + head xs
