
module AMM.Payoffs where

import OpenGames.Engine.Engine
import AMM.Types
import qualified Data.Map.Strict as M

{-
Defines payoffs for players
-}

----------
-- Payoffs
----------

-- Compute payoff for coordinator by maximizing the fee
-- Compute fee payment by relevant player
-- NOTE We assume here that only the first transaction has to pay a fee
computePayoffCoordinatorMaxFee :: (MapTransactionResults, MapPlayerUtility) -> (CoordinatorPayoff, (PlayerID,Fee))
computePayoffCoordinatorMaxFee (mapResults,_) =
  let ls = M.toList mapResults
      (playerID, (_,_,fee)) = head ls
      in (fee,(playerID, -fee))

-- Compute payoff for coordinator by maximizing utility of players
-- Compute fee payment by relevant player
-- NOTE We assume here that only the first transaction has to pay a fee
computePayoffCoordinatorMaxPlayerUtility :: (MapTransactionResults, MapPlayerUtility) -> (CoordinatorPayoff, (PlayerID,Fee))
computePayoffCoordinatorMaxPlayerUtility (mapResults,mapUtility) =
  let ls = M.toList mapResults
      (playerID, (_,_,fee)) = head ls
      sumUtility = M.foldr (+) 0 mapUtility
      in (sumUtility, (playerID, -fee))

-- Compute Utility Map for all players in denomination of the first currency
computePayoffPlayerMap
  :: Ord k =>
     ContractState
     -> (M.Map k (Double, Double), M.Map k (SwapTransaction, b1),
         M.Map k (Result, b2, c))
     -> M.Map k Double
computePayoffPlayerMap contractState (mapEndowments, mapTransactions ,mapResults) =
  let updatePayoffSinglePlayer k = updateBalance contractState (mapTransactions M.! k) (mapResults M.! k)
      in M.mapWithKey updatePayoffSinglePlayer mapEndowments

-- Update individual balance and evaluate in terms of first currency
updateBalance contractState (swapTransaction,_) (result,_,_) (curr0,curr1) =
  let newBalance =
        case swapTransaction of
          Swap0 sent0 ->
            case result of
              Swap0Out received0 -> (curr0 - sent0 + received0, curr1) -- ^ case should not happen
              Swap1Out received1 -> (curr0 - sent0, curr1 + received1)
          Swap1 sent1 ->
            case result of
              Swap0Out received0 -> (curr0 + received0, curr1 - sent1)
              Swap1Out received1 -> (curr0, curr1 - sent1 + received1)  -- ^ case should not happen
        in  denominateInFirstCurrency contractState newBalance
  where
    denominateInFirstCurrency contractState (x1,x2) = x1 + x2 * (snd contractState / fst contractState)

-- Project out payoffs for two players
projectPlayerPayoff :: Ord k => k -> M.Map k a -> a
projectPlayerPayoff name utilityMap =
 utilityMap M.! name 
