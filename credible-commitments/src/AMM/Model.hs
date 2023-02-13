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

{-
Provides the complete open game
-}


completeGame exchangeRate name1 name2 upperBound goalFunctionCoordinator actionSpaceTXs1 actionSpaceTXs2 privateValueDistribution1 privateValueDistribution2 = [opengame|
  inputs: mapEndowments, state ;
  feedback: ;

  :------:
  inputs : state ;
  operation : players name1 name2 upperBound actionSpaceTXs1 actionSpaceTXs2 privateValueDistribution1 privateValueDistribution2;
  outputs : transactionsSubmitted, lsPrivateValues;
  returns : ;

  inputs : transactionsSubmitted, state ;
  operation : coordinator ;
  outputs : lsTransactionOrdered;
  returns : payoffCoordinator;

  inputs : lsTransactionOrdered, state ;
  operation : amm ;
  outputs : lsResults;

  inputs : mapEndowments, lsTransactionOrdered, lsResults, lsPrivateValues ;
  operation : payoffsCoordinator exchangeRate goalFunctionCoordinator ;
  outputs : payoffCoordinator ;

  inputs : mapEndowments, lsTransactionOrdered, lsResults, lsPrivateValues ;
  operation : payoffPlayers exchangeRate name1 name2 ;

  :------:
  outputs :  ;
  returns : ;

|]
