
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
computePayoffCoordinatorMaxFee :: (TransactionResultsLS, MapPlayerUtility) -> (CoordinatorPayoff, (PlayerID,Fee))
computePayoffCoordinatorMaxFee (ls,_) =
  let (playerID, (_,_,fee)) = head ls
      in (fee,(playerID, -fee))

-- Compute payoff for coordinator by maximizing utility of players
-- Compute fee payment by relevant player
-- NOTE We assume here that only the first transaction has to pay a fee
computePayoffCoordinatorMaxPlayerUtility :: (TransactionResultsLS, MapPlayerUtility) -> (CoordinatorPayoff, (PlayerID,Fee))
computePayoffCoordinatorMaxPlayerUtility (ls,mapUtility) =
  let (playerID, (_,_,fee)) = head ls
      sumUtility = M.foldr (+) 0  mapUtility 
      in (sumUtility, (playerID, -fee))

-- Compute Utility Map for all players in denomination of the first currency
computePayoffPlayerMap
  :: ContractState
     -> (MapPlayerEndowment
        ,TransactionsLS
        ,TransactionResultsLS
        ,PrivateValuesLS)
     -> MapPlayerUtility
computePayoffPlayerMap contractState (mapEndowments, lsTransactions ,lsResults, lsPrivateValues) =
  let updatePayoffSinglePlayer playerName =
        let findTX = snd $ head $ filter (\(name,tx) -> name == playerName) lsTransactions
            findResults = snd $ head $ filter (\(name,result) -> name == playerName) lsResults
            privateValue = addPrivateValue playerName lsTransactions lsPrivateValues
            in updateBalance contractState findTX  findResults privateValue
                in M.mapWithKey updatePayoffSinglePlayer mapEndowments

-- Update individual balance and evaluate in terms of first currency
updateBalance contractState (swapTransaction,_) (result,_,_) privateValue (endowment0,endowment1)  =
  let newBalance =
        case swapTransaction of
          Swap0 sent0 ->
            case result of
              Swap0Out received0 -> (endowment0 - sent0 + received0 + privateValue, endowment1) -- ^ case should not happen
              Swap1Out received1 -> (endowment0 - sent0 + privateValue, endowment1 + received1)
          Swap1 sent1 ->
            case result of
              Swap0Out received0 -> (endowment0 + received0 + privateValue, endowment1 - sent1)
              Swap1Out received1 -> (endowment0 + privateValue, endowment1 - sent1 + received1)  -- ^ case should not happen
        in  denominateInFirstCurrency contractState newBalance
  where
    denominateInFirstCurrency contractState (x1,x2) = x1 + x2 * (snd contractState / fst contractState)

-- Project out payoffs for two players
projectPlayerPayoff name utilityMap = utilityMap M.! name

-- Add private value if first tx happens to be first
addPrivateValue playerName lsTransactions lsPrivateValues =
  if position == 1
     then privateValue
     else 0
  where privateValue = snd $ head $ filter (\(name,tx) -> name == playerName) lsPrivateValues
        position = findValue lsTransactions playerName
        findValue [] _ = 0
        findValue ((name,_):xs) playerName =
          if name == playerName
            then 1
            else 1 + findValue xs playerName

