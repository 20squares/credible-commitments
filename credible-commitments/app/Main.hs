module Main (main) where

import AMM.Analytics
import AMM.Parameterization

main :: IO ()
main = do
  putStrLn "Run parameterized analytics for AMM game - max utility"
  mainAMMMaxUtility
  putStrLn "Run parameterized analytics for AMM game - manual utility"
  mainAMMManual
  putStrLn "Identify eq pair of fees"
  print idFee
  putStrLn "Show output for these fees"
  mapM_ mainAMMGreedy idFee

-- Identify the equilibrium for a pair of fees
mainAMMGreedyFindEqFee (fee1,fee2) = do
  findEqCompleteGame (testStrategiesGreedy fee1 fee2) testParametersGreedy
-- Run it on the available grid 
idFee = [fee | fee <- (fmap (\x -> (x,x)) [0,1..50]), mainAMMGreedyFindEqFee fee ]

-- Show output for fee 
mainAMMGreedy (fee1,fee2) = do
  putStrLn $ "With following fees" ++ (show (fee1,fee2))
  printOutputCompleteGame (testStrategiesGreedy fee1 fee2) testParametersGreedy

mainAMMMaxUtility = printEquilibriumCompleteGame testStrategiesMaxUtility testParametersMaxUtility

mainAMMManual = printEquilibriumCompleteGame testStrategiesManual testParametersMaxUtility

