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


completeGame exchangeRate name1 name2 upperBound goalFunctionCoordinator actionSpaceTXs1 actionSpaceTXs2 = [opengame|
  inputs: mapEndowments, state ;
  feedback: ;

  :------:
  inputs : state ;
  operation : players name1 name2 upperBound actionSpaceTXs1 actionSpaceTXs2;
  outputs : transactionsSubmitted;
  returns : ;

  inputs : transactionsSubmitted, state ;
  operation : coordinator ;
  outputs : mapTransactionOrdered;
  returns : payoffCoordinator;

  inputs : mapTransactionOrdered, state ;
  operation : amm ;
  outputs : mapResults;

  inputs : mapEndowments, mapTransactionOrdered, mapResults ;
  operation : payoffsCoordinator exchangeRate goalFunctionCoordinator ;
  outputs : payoffCoordinator ;

  inputs : mapEndowments, mapTransactionOrdered, mapResults ;
  operation : payoffPlayers exchangeRate name1 name2 ;

  :------:
  outputs :  ;
  returns : ;

|]
