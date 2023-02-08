module Main (main) where

import AMM.Analytics
import AMM.Parameterization

main :: IO ()
main = do
  putStrLn "Run parameterized analytics for AMM game - greedy"
  mainAMMGreedy
  putStrLn "Run parameterized analytics for AMM game - max utility"
  mainAMMMaxUtility



mainAMMGreedy = printEquilibriumCompleteGame testStrategiesGreedy testParametersGreedy

mainAMMMaxUtility = printEquilibriumCompleteGame testStrategiesMaxUtility testParametersMaxUtility
