module Main (main) where

import AMM.Analytics
import AMM.Parameterization

main :: IO ()
main = do
  putStrLn "Run parameterized analytics for AMM game"
  mainAMM


mainAMM = printEquilibriumCompleteGame testStrategiesGreedy testParametersGreedy
