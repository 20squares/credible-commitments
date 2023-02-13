{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}


module AMM.Components where

import AMM.ActionSpaces
import AMM.AMM
import AMM.Payoffs
import AMM.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-
Provides  the open game components
-}


{-
TODO
- We probably should start with 2 transactions only to keep it as close as possible to the PD
- These transactions should be identical (this should feature the PD)
- We assume that only the first transaction pays a reward to the coordinator
- What choices should the players make?
   - Choose from a subset of transactions; maybe two transactions each? Again, similar to the PD
   - Choose bid for coordinator
-}


-------------
-- Open games
-------------

-- NOTE: We assume for now that the builder is not including own transactions

-- Single player chooses which transaction to send to coordinator and with what fee
chooseTransactionAndFee name upperBound actionSpaceTXs privateValueDistribution=
  [opengame|
  inputs: state ;
  feedback: ;

  :------:

  inputs :   ;
  operation : nature privateValueDistribution ;
  outputs : priorityValue ;
  returns :  ;

  inputs :  state ;
  operation : dependentDecision name $ const $ actionSpaceTXs ;
  outputs : tx;
  returns : 0 ;

  inputs : state, tx, priorityValue ;
  operation : dependentDecision name $ const $ actionSpaceFee upperBound ;
  outputs : fee;
  returns : 0 ;

  inputs : tx,fee ;
  operation : forwardFunction $ uncurry combineTXAndFee ; 
  outputs : txWithFee ;

  :------:
  outputs : txWithFee, priorityValue ;
  returns : ;

|]

-- Two players choose their transactions and fee
players name1 name2 upperBound actionSpaceTXs1 actionSpaceTXs2  privateValueDistribution1 privateValueDistribution2 =
  [opengame|
  inputs: state ;
  feedback: ;

  :------:

  inputs :  state ;
  operation : chooseTransactionAndFee name1 upperBound actionSpaceTXs1 privateValueDistribution1  ;
  outputs : txWithFee1,priorityValue1;

  inputs :  state ;
  operation : chooseTransactionAndFee name2 upperBound actionSpaceTXs2 privateValueDistribution2;
  outputs : txWithFee2, priorityValue2;

  inputs : txWithFee1, txWithFee2 ;
  operation : forwardFunction $  combineTXIntoList name1 name2;
  outputs : transactionsSubmitted ;

  inputs : priorityValue1, priorityValue2 ;
  operation : forwardFunction $  combinePrivateValuesIntoList name1 name2;
  outputs : privateValuesLs ;

  :------:
  outputs : transactionsSubmitted, privateValuesLs ;
  returns : ;

|]

coordinator  = [opengame|
  inputs: transactionsSubmitted, state ;
  feedback: ;

  :------:

  inputs :     transactionsSubmitted, state ;
  operation :  dependentDecision "coordinator" $ actionSpaceCoordinator  ;
  // We initiate the function with a zero fee ;
  outputs :    lsTransactionOrdered;
  returns :    payoffCoordinator ;


  :------:
  outputs : lsTransactionOrdered;
  returns : payoffCoordinator;
|]

-- Amm functionality
amm  = [opengame|
  inputs: lsTransactionsOrdered, state ;
  feedback: ;

  :------:

  inputs : lsTransactionsOrdered, state ;
  operation : forwardFunction $  mapSwapsWithAmounts ;
  outputs : lsResults;

  :------:
  outputs : lsResults;
  returns : ;
|]

-- Payoffs for coordinator
payoffsCoordinator exchangeRate goalFunction = [opengame|
  inputs:  mapEndowments, lsTransactions,lsResults, privateValuesLs ;
  feedback: ;

  :------:

  inputs : mapEndowments, lsTransactions, lsResults, privateValuesLs ;
  operation : forwardFunction $ computePayoffPlayerMap exchangeRate;
  outputs : utilityMap ;
  // Repeat component here for localizing the information

  inputs : lsResults, utilityMap ;
  operation : forwardFunction $ goalFunction ;
  outputs : payoffCoordinator, payoffPlayer ;

  inputs :  payoffPlayer ;
  operation : addRolePayoffs ;
  outputs : ;
  // This takes care of which player has to pay the fee

  :------:
  outputs : payoffCoordinator ;
  returns : ;
|]




-- Payoffs for a single player
payoffSinglePlayer name = [opengame|
  inputs: utilityMap ;
  feedback: ;

  :------:

  inputs : utilityMap ;
  operation : forwardFunction $ projectPlayerPayoff name;
  outputs : payoffPlayer ;

  inputs :  payoffPlayer ;
  operation : addPayoffs name ;
  outputs : ;

  :------:
  outputs :  ;
  returns : ;
|]

-- Payoffs for all players 
payoffPlayers exchangeRate name1 name2 = [opengame|
  inputs:  mapEndowments, mapTransactions, mapResults, privateValuesLs;
  feedback: ;

  :------:

  inputs : mapEndowments, mapTransactions, mapResults, privateValuesLs ;
  operation : forwardFunction $ computePayoffPlayerMap exchangeRate;
  outputs : utilityMap ;

  inputs :  utilityMap ;
  operation : payoffSinglePlayer name1 ;

  inputs :  utilityMap ;
  operation : payoffSinglePlayer name2 ;

  :------:
  outputs : ;
  returns : ;
|]



