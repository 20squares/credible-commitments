{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.Parameterization where

import AMM.Payoffs
import AMM.Strategies
import AMM.Types
import qualified Dhall.Map as M

{-
Instantiates the parameters used in the game 
-}

-------------
-- Parameters
-------------


-- Define action space for choosing a tx
actionSpace1 = [Swap0 50]
actionSpace2 = [Swap0 40]

player1 = "player1"
player2 = "player2"

testEndowments = M.fromList [(player1,(50,50)),(player2,(50,50))]

testStrategiesGreedy = strategyTupleMaxFee (Swap0 50) (Swap0 40) 0 2

testParametersGreedy = Parameters
  (100,100)
  player1
  player2
  50
  actionSpace1
  actionSpace2
  computePayoffCoordinatorMaxFee
  testEndowments


testParametersSocial = Parameters
  (100,100)
  player1
  player2
  50
  actionSpace1
  actionSpace2
  computePayoffCoordinatorMaxPlayerUtility
  testEndowments

