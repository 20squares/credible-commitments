{-# LANGUAGE RecordWildCards #-}

module AMM.Strategies where

import OpenGames.Engine.Engine
import AMM.ActionSpaces
import AMM.AMM
import AMM.Payoffs
import AMM.Types
import Data.List (maximumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)

import Numeric.Probability.Distribution (expected)

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
          (ContractState, SwapTransaction, PrivateValue)
          Fee
strategyFee fee = pureAction fee

-----------------------
-- Strategy coordinator
-----------------------

-- Maximize fee received by players
maxFeeStrategy
  :: Kleisli
       Stochastic
          (TransactionsLS, ContractState)
          TransactionsLS
maxFeeStrategy = Kleisli
 (\observation ->
    let ls = chooseMaximalFee $ transformLs $ actionSpaceCoordinator observation
        in if length ls == 1
              then playDeterministically $ fst $ head ls -- ^ if only one element, play deterministically
              else uniformDist $ fmap fst ls)            -- ^ if several elements, randomize uniformly

-- Transform into pair of (fee,tx)
transformLs :: [[(PlayerID,Transaction)]] -> [(TransactionsLS, Fee)]
transformLs ls = [(x, snd . snd . head $ x)| x <- ls]


-- Filter the list by the maximum elements
chooseMaximalFee
  :: [(TransactionsLS, Fee)] -> [(TransactionsLS, Fee)]
chooseMaximalFee ls =
  filter  (\(_,x) -> x == findMaximalElement ls) ls
  where
    findMaximalElement :: [(TransactionsLS, Fee)] -> Fee
    findMaximalElement ls = snd $ maximumBy (comparing snd) ls

-- Maximize utility of players
maxUtilityStrategy
  :: Parameters
  -> Kleisli
       Stochastic
          (TransactionsLS, ContractState)
          TransactionsLS
maxUtilityStrategy Parameters{..} = Kleisli
 (\observation -> 
    let actionLS  = actionSpaceCoordinator observation
        contractState     = snd observation
        actionLS' = [(contractState,txs)| txs <- actionLS]
        results   = [(contractState,mapEndowments, txs, mapSwapsWithAmounts (txs,state))| (state,txs) <- actionLS']
        expectedValue1 = expected privateValueDistribution1
        expectedValue2 = expected privateValueDistribution2
        utilityLS = [(txs, computePayoffPlayerMap contractState (mapEndowments,txs,resultsTXs,[("player1",expectedValue1),("player2",expectedValue2)]))| (state,mapEndowments, txs, resultsTXs) <- results] 
        chooseMaximalUtility = fst $ maximumBy (comparing snd) [(txs, M.foldr (+) 0 $ utility)| (txs,utility) <- utilityLS]
        in playDeterministically $ chooseMaximalUtility)

-- Provide manual strategy input for exploration
manualStrategy
  :: TransactionsLS 
  -> MapPlayerEndowment
  -> Kleisli
       Stochastic
          (TransactionsLS, ContractState)
          TransactionsLS
manualStrategy ls _ =
  pureAction ls

-- Composing strategy tuple max fee
strategyTupleMaxFee swap1 swap2 fee1 fee2  =
  strategySwap swap1        -- Player 1 swap tx
  ::- strategyFee fee1      -- Player 1 coinbase.transfer
  ::- strategySwap swap2    -- Player 2 swap tx
  ::- strategyFee fee2      -- Player 2 coinbase.transfer
  ::- maxFeeStrategy        -- Coordinator strategy
  ::- Nil

-- Composing strategy tuple max utility
strategyTupleMaxUtility swap1 swap2 fee1 fee2 par =
  strategySwap swap1          -- Player 1 swap tx
  ::- strategyFee fee1        -- Player 1 coinbase.transfer
  ::- strategySwap swap2      -- Player 2 swap tx
  ::- strategyFee fee2        -- Player 2 coinbase.transfer
  ::- maxUtilityStrategy par  -- Coordinator strategy
  ::- Nil

-- Composing strategy tuple max utility
strategyTupleManualCoordinator swap1 swap2 fee1 fee2 endowmentMap ls =
  strategySwap swap1                   -- Player 1 swap tx
  ::- strategyFee fee1                 -- Player 1 coinbase.transfer
  ::- strategySwap swap2               -- Player 2 swap tx
  ::- strategyFee fee2                 -- Player 2 coinbase.transfer
  ::- manualStrategy ls endowmentMap  -- Coordinator strategy
  ::- Nil

