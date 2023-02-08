{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.Parameterization where

import AMM.Payoffs
import AMM.Strategies
import AMM.Types
import qualified Data.Map.Strict as M

{-
Instantiates the parameters used in the game 
-}

-------------
-- Parameters
-------------

player1 = "player1"
player2 = "player2"

testEndowments = M.fromList [(player1,(50,50)),(player2,(50,50))]


testParameters = Parameters
  (100,100)
  player1
  player2
  50
  computePayoffCoordinatorMaxPlayerUtility
  testEndowments

testStrategies = strategyTupleMaxFee (Swap0 20) (Swap0 20) 0 0
