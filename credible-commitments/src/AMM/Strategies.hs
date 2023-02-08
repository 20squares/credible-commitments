{-# LANGUAGE RecordWildCards #-}

module AMM.Strategies where

import OpenGames.Engine.Engine
import AMM.ActionSpaces
import AMM.AMM
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

-- Strategy coordinator
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
      fst $ maximumBy (comparing snd) [(x, snd . head . M.toList $ x)| x <- lsOfMaps]


-- Complete tuple
strategyTupleMaxFee swap1 swap2 fee1 fee2  = strategySwap swap1 ::-  strategyFee fee1 ::- strategySwap swap2 ::- strategyFee fee2 ::- maxFeeStrategy ::- Nil
