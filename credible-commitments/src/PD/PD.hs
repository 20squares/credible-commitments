{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PD.PD where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor


--------------
-- 0. Overview
{-
This module contains simple pd games including ones with credible commitments
-}

-----------------------
-- 1. Types and payoffs

-- Auxiliary definitions for avoiding parser complications
commitmentChoice = Left ()
pdChoice         = Right ()
empty            = ()

-- Avaliable actions
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> (Payoff,Payoff)
prisonersDilemmaMatrix Cooperate Cooperate   = (3,3)
prisonersDilemmaMatrix Cooperate Defect  = (0,3)
prisonersDilemmaMatrix Defect Cooperate  = (3,0)
prisonersDilemmaMatrix Defect Defect = (1,1)

--------------------
-- 2. Representation
-- 2.0 Prisoner's dilemma
-- NOTE We make this more verbose than needed so that we can also export the payoff if we want to
prisonersDilemmaGame = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [Cooperate,Defect]);
   outputs   : decisionAlice ;
   returns   : fst payoffTuple ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [Cooperate,Defect]);
   outputs   : decisionBob ;
   returns   : snd payoffTuple ;

   inputs    : decisionAlice, decisionBob  ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ prisonersDilemmaMatrix ;
   outputs   : payoffTuple;
   returns   : ;



   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- 2.0.1 Prisoner's dilemma, exhogenous payoff matrix
-- I'm passing payoff matrix as an exhogenous parameter, useful in 2.2.1.
prisonersDilemmaGameExhogenous matrix = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [Cooperate,Defect]);
   outputs   : decisionAlice ;
   returns   : fst payoffTuple ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [Cooperate,Defect]);
   outputs   : decisionBob ;
   returns   : snd payoffTuple ;

   inputs    : decisionAlice, decisionBob  ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ matrix ;
   outputs   : payoffTuple;
   returns   : ;



   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]
-- 2.1 Prisoner's dilemma with commitment device; modelled as a function
-- NOTE There are other options - explore this if of interest
prisonersDilemmaBobUnderCommitment aliceCommitment = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [Cooperate,Defect]);
   outputs   : decisionBob ;
   returns   : payoffBob ;

   inputs    : decisionBob ;
   feedback  :      ;
   operation : forwardFunction $ aliceCommitment ;
   outputs   : decisionAlice ;
   returns   : ;
   // Alice's strategic choice is substituted by a computation

   inputs    : decisionAlice, decisionBob  ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ prisonersDilemmaMatrix ;
   outputs   : (payoffAlice,payoffBob);
   returns   : ;

   inputs    : payoffAlice  ;
   feedback  : ;
   operation : addPayoffs "Alice" ;
   outputs   : ;
   returns   : ;
   // We are doing book-keeping for Alice in case we want to embedd that game into a larger component

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- combined branching game
-- NOTE this is a branching game; only one of the possible branches will be played
branchingGame aliceCommitment = (prisonersDilemmaBobUnderCommitment aliceCommitment) +++ prisonersDilemmaGame

prisonersDilemmaAliceChoice aliceCommitment = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [commitmentChoice,pdChoice]);
   outputs   : gameDecisionAlice ;
   returns   : 0 ;

   inputs    : gameDecisionAlice ;
   feedback  :      ;
   operation : branchingGame aliceCommitment;
   outputs   : discard;
   returns   : ;
   // discard the output

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- 2.2 Prisoner's dilemma with commitment device _plus_ additional transfer by Bob
-- NOTE There are other options - explore this if of interest
prisonersDilemmaBobUnderCommitmentTransfer aliceCommitment = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [Cooperate,Defect]);
   outputs   : decisionBobCooperate ;
   returns   : payoffBob - decisionBobTransfer;

   inputs    : decisionBobCooperate ;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [0,1,2,3]);
   outputs   : decisionBobTransfer ;
   returns   : payoffBob - decisionBobTransfer;
   // Bob's strategy is conditional on his cooperate decision before

   inputs    : (decisionBobCooperate,decisionBobTransfer) ;
   feedback  :      ;
   operation : forwardFunction $ aliceCommitment ;
   outputs   : decisionAlice ;
   returns   : ;
   // Alice's strategic choice is substituted by a computation

   inputs    : decisionAlice, decisionBobCooperate ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ prisonersDilemmaMatrix ;
   outputs   : (payoffAlice,payoffBob);
   returns   : ;

   inputs    : payoffAlice  ;
   feedback  : ;
   operation : addPayoffs "Alice" ;
   outputs   : ;
   returns   : ;
   // We are doing book-keeping for Alice in case we want to embedd that game into a larger component

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- combined branching game
-- NOTE this is a branching game; only one of the possible branches will be played
branchingGameTransfer aliceCommitment = (prisonersDilemmaBobUnderCommitmentTransfer aliceCommitment) +++ prisonersDilemmaGame

prisonersDilemmaAliceChoiceTransfer aliceCommitment = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [commitmentChoice,pdChoice]);
   outputs   : gameDecisionAlice ;
   returns   : 0 ;

   inputs    : gameDecisionAlice ;
   feedback  :      ;
   operation : branchingGameTransfer aliceCommitment;
   outputs   : discard;
   returns   : ;
   // discard the output

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- 2.2.1 Prisoner's dilemma with commitment device _plus_ additional transfer by Bob, bribe customizable
-- NOTE There are other options - explore this if of interest
prisonersDilemmaBobUnderCommitmentTransferBribe aliceCommitment payoffMatrix = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:

   inputs    : ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [0,1,2,3]);
   outputs   : decisionAliceBribe ;
   returns   : payoffAlice + decisionBobTransfer;

   inputs    : decisionAliceBribe, payoffMatrix;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [Cooperate,Defect]);
   outputs   : decisionBobCooperate ;
   returns   : payoffBob - decisionBobTransfer;
   

   inputs    : decisionBobCooperate, decisionAliceBribe;
   feedback  :      ;
   operation : dependentDecision "Bob" (const [0,1,2,3,4,5]);
   outputs   : decisionBobTransfer ;
   returns   : payoffBob - decisionBobTransfer;
   // Bob's strategy is conditional on his cooperate decision before


   inputs    : (decisionBobCooperate,decisionBobTransfer,decisionAliceBribe) ;
   feedback  :      ;
   operation : forwardFunction $ aliceCommitment ;
   outputs   : decisionAlice ;
   returns   : ;
   // Alice's strategic choice is substituted by a computation

   inputs    : decisionAlice, decisionBobCooperate ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ payoffMatrix ;
   outputs   : (payoffAlice,payoffBob);
   returns   : ;


   // We are doing book-keeping for Alice in case we want to embedd that game into a larger component

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- combined branching game
-- NOTE this is a branching game; only one of the possible branches will be played
branchingGameTransferBribe aliceCommitment matrix = (prisonersDilemmaBobUnderCommitmentTransferBribe aliceCommitment matrix) +++ prisonersDilemmaGameExhogenous matrix

prisonersDilemmaAliceChoiceTransferBribe aliceCommitment matrix = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Alice" (const [commitmentChoice,pdChoice]);
   outputs   : gameDecisionAlice ;
   returns   : 0 ;

   inputs    : gameDecisionAlice ;
   feedback  :      ;
   operation : branchingGameTransferBribe aliceCommitment matrix;
   outputs   : discard;
   returns   : ;
   // discard the output

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

