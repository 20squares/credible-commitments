{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.AMM where

import AMM.Types
import qualified Data.Map.Strict as M

{-
Provides basic functionality for an AMM
-}

-------------------------
-- 1. Auxiliary functions

inRange :: Double -> Double -> Bool
inRange _ _ = True

-- Swap a given amount of tokens
swapWithAmount :: Transaction -> ContractState -> TransactionResult
swapWithAmount ((Swap0 amt), fee) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
          ( Swap1Out (reserve1 - ((reserve0 * reserve1) / (reserve0 + amt) + 1))
          , ( reserve0 + amt, (reserve0 * reserve1) / (reserve0 + amt) + 1)
          , fee)
      else (Swap1Out 0, st, fee) -- ^ The fee is paid anyways? 
swapWithAmount ((Swap1 amt), fee) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
     ( Swap0Out (reserve0 - ((reserve0 * reserve1) / (reserve1 + amt) + 1))
     , ( (reserve0 * reserve1) / (reserve1 + amt) + 1, reserve1 + amt)
     , fee)
      else (Swap0Out 0, st, fee) -- ^ The fee is paid anyways?

-- Thread through a sequence of transactions
-- NOTE: We assume that the first element in the map is the first transaction to be run.
-- NOTE: The latest element in the outcomes map is the latest state.
mapSwapsWithAmounts :: (MapTransactions, ContractState) -> MapTransactionResults
mapSwapsWithAmounts (mapTransactions, state) =
  let ls = M.toList mapTransactions
      resultsLs = lsSwapsWithAmounts (ls,state)
      in M.fromList resultsLs
  where
      lsSwapsWithAmounts :: ([(PlayerID,Transaction)], ContractState) -> [(PlayerID,TransactionResult)]
      lsSwapsWithAmounts ([],_) = []
      lsSwapsWithAmounts ((id,tx):xs,state) =
        let x@(result, stateNew, fee) = swapWithAmount tx state 
            in (id,x) : lsSwapsWithAmounts (xs,stateNew)
