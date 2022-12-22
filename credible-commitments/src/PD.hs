{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PD where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor


--------------
-- 0. Overview
-- This file contains three simple simultaneous move games: prisoner dilemma (a social dilemma), meeting in new york (coordination game), and matching pennies (anti-coordination game)



-----------------------
-- 1. Types and payoffs

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> (Payoff,Payoff)
prisonersDilemmaMatrix Cooperate Cooperate   = (2,2)
prisonersDilemmaMatrix Cooperate Defect  = (0,3)
prisonersDilemmaMatrix Defect Cooperate  = (3,0)
prisonersDilemmaMatrix Defect Defect = (1,1)



--------------------
-- 1. Representation
-- 1.0 Prisoner's dilemma
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

-- 1.1 Prisoner's dilemma with commitment device; modelled as a function
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

-- 1.3. combined branching game
-- NOTE this is a branching game; only one of the both will be played
branchingGame aliceCommitment = (prisonersDilemmaBobUnderCommitment aliceCommitment) +++ prisonersDilemmaGame

-- auxiliary definitions for avoiding parser complications
commitmentChoice = Left ()
pdChoice         = Right ()


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


