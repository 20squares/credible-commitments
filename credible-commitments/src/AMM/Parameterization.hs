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
  (100,100)                                 -- Initial AMM exchange rate
  player1                                   -- Alice
  player2                                   -- Bob
  50                                        -- Max coinbase.transfer for Coordinator
  computePayoffCoordinatorMaxPlayerUtility  -- The goal function for Coordinator. Basically how greedy/altruistic Coordinator is.
  testEndowments                            -- Initial player endowments

testStrategies = strategyTupleMaxFee (Swap0 20) (Swap0 20) 0 0