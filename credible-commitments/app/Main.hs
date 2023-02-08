module Main (main) where

import AMM.Analytics
import AMM.Parameterization

main :: IO ()
main = printEquilibriumCompleteGame testStrategies testParameters
