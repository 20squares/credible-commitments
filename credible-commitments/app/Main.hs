module Main (main) where

import AMM.Analytics
import AMM.Parameterization
import PD.Analytics
import PD.Coordinator
import PD.PD
import PD.Strategies

main :: IO ()
main = do
  putStrLn "//////////PD//////////"
  mainPD
  putStrLn "//////////AMM//////////"
  mainAMM



-----
-- PD
-----

mainPD = do
  putStrLn "1. Vanilla prioner's dilemma"
  isEquilibriumPrisonersDilemma strategyTupleDefect
  putStrLn "2. PRisoner's dilemma with a commitment device"
  isEquilibriumPrisonersDilemmaCommitment conditionalCooperate bobStrategyCooperate
  putStrLn "3. Prisoner's dilemma with branching"
  isEquilibriumPrisonersDilemmaAliceChoice conditionalCooperate strategyTupleCommit
  putStrLn "4. Prisoner's dilemma with extortion"
  isEquilibriumPrisonersDilemmaAliceChoiceTransfer conditionalCooperateTransfer strategyTupleCommitTransfer
  putStrLn "5. Prisoner's dilemma with a coordinator"
  isEquilibriumPrisonersDilemmaCoordinator conditionalCooperateTransfer strategyTupleCoordinator

------
-- AMM
------

mainAMM = do
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

