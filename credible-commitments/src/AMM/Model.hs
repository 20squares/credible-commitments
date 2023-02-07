{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.Model where

import AMM.ActionSpaces
import AMM.AMM
import AMM.Components
import AMM.Payoffs
import AMM.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import Data.List (permutations)

{-
Provides the complete open game
-}


completeGame
  :: ContractState
     -> String
     -> String
     -> Fee
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[Kleisli Stochastic ContractState SwapTransaction,
            Kleisli Stochastic (ContractState, SwapTransaction) Fee,
            Kleisli Stochastic ContractState SwapTransaction,
            Kleisli Stochastic (ContractState, SwapTransaction) Fee,
            Kleisli
              Stochastic (MapTransactions, ContractState) MapTransactions]
          '[[DiagnosticInfoBayesian ContractState SwapTransaction],
            [DiagnosticInfoBayesian (ContractState, SwapTransaction) Fee],
            [DiagnosticInfoBayesian ContractState SwapTransaction],
            [DiagnosticInfoBayesian (ContractState, SwapTransaction) Fee],
            [DiagnosticInfoBayesian
               (MapTransactions, ContractState) MapTransactions]]
          (Data.Map.Internal.Map String (Double, Double), ContractState)
          ()
          ()
          ()
completeGame exchangeRate name1 name2 upperBound = [opengame|
  inputs: mapEndowments, state ;
  feedback: ;

  :------:
  inputs : state ;
  operation : players name1 name2 upperBound ; 
  outputs : transactionsSubmitted;
  returns : ;

  inputs : transactionsSubmitted, state ;
  operation : coordinator ; 
  outputs : mapTransactionOrdered;
  returns : payoffCoordinator;

  inputs : mapTransactionOrdered, state ;
  operation : amm ;
  outputs : mapResults;

  inputs : mapResults ;
  operation : payoffsCoordinator ; 
  outputs : payoffCoordinator ;

  inputs : mapEndowments, mapTransactionOrdered, mapResults ;
  operation : payoffPlayers exchangeRate name1 name2 ; 

  :------:
  outputs :  ;
  returns : ;

|]
