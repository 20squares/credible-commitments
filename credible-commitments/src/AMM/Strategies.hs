{-# LANGUAGE RecordWildCards #-}

module AMM.Strategies where

import OpenGames.Engine.Engine
import AMM.ActionSpaces
import AMM.AMM
import AMM.Payoffs
import AMM.Types
import Data.List (maximumBy)
import qualified Dhall.Map as M
import Data.Ord (comparing)

{-
Defines concrete strategies
-}

-------------
-- Strategies
-------------

-- Strategy swap
strategySwap
  :: SwapTransaction
     -> Kleisli
          Stochastic
          ContractState
          SwapTransaction
strategySwap swap = pureAction swap

-- Strategy fee
strategyFee
  :: Fee
     -> Kleisli
          Stochastic
          (ContractState, SwapTransaction)
          Fee
strategyFee fee = pureAction fee

-----------------------
-- Strategy coordinator

-- Maximize fee received by players
maxFeeStrategy
  :: Kleisli
       Stochastic
          (MapTransactions, ContractState)
          MapTransactions
maxFeeStrategy = Kleisli
 (\observation -> playDeterministically $ chooseMaximalFee $ actionSpaceCoordinator observation)
 where
    chooseMaximalFee :: [MapTransactions] -> MapTransactions
    chooseMaximalFee lsOfMaps =
      fst $ maximumBy (comparing snd) [(x, snd . snd . head . M.toList $ x)| x <- lsOfMaps]

-- Maximize utility of players
maxUtilityStrategy
  :: MapPlayerEndowment
  -> Kleisli
       Stochastic
          (MapTransactions, ContractState)
          MapTransactions
maxUtilityStrategy endowment = Kleisli
 (\observation -> 
    let actionLS  = actionSpaceCoordinator observation
        contractState     = snd observation
        actionLS' = [(contractState,txs)| txs <- actionLS]
        results   = [(contractState,endowment, txs, mapSwapsWithAmounts (txs,state))| (state,txs) <- actionLS']
        utilityLS = [(txs, computePayoffPlayerMap contractState (endowment,txs,resultsTXs))| (state,endowment, txs, resultsTXs) <- results]
        chooseMaximalUtility = fst $ maximumBy (comparing snd) [(txs, foldr (+) 0 $ fmap snd $ M.toList utility)| (txs,utility) <- utilityLS]
        in playDeterministically $ chooseMaximalUtility)



-- Composing strategy tuple
strategyTupleMaxFee swap1 swap2 fee1 fee2  = 
  strategySwap swap1        -- Player 1 swap tx
  ::- strategyFee fee1      -- Player 1 coinbase.transfer
  ::- strategySwap swap2    -- Player 2 swap tx
  ::- strategyFee fee2      -- Player 2 coinbase.transfer
  ::- maxFeeStrategy        -- Coordinator strategy
  ::- Nil
