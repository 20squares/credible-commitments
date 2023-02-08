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

testStrategiesGreedy = strategyTupleMaxFee (Swap0 50) (Swap0 40) 0 0.5

testParametersGreedy = Parameters
  (100,100)                                 -- Initial AMM exchange rate
  player1                                   -- Alice
  player2                                   -- Bob
  50                                        -- Max coinbase.transfer for Coordinator
  actionSpace1                              -- Action space available for player1
  actionSpace2                              -- Action space available for player2
  computePayoffCoordinatorMaxFee            -- Coordinator's goal function: maximizes fees
  testEndowments                            -- Initial player endowments


testParametersSocial = Parameters
  (100,100)                                 -- Initial AMM exchange rate
  player1                                   -- Alice
  player2                                   -- Bob
  50                                        -- Max coinbase.transfer for Coordinator
  actionSpace1                              -- Action space available for player1
  actionSpace2                              -- Action space available for player2
  computePayoffCoordinatorMaxPlayerUtility  -- Coordinator's goal function: maximize sum of players' utility
  testEndowments                            -- Initial player endowments
