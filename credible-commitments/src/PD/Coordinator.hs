{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PD.Coordinator where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import PD

--------------
-- 0. Overview

{-
This module contains a model with coordinator
-}

---------------------------------------
-- 1. Auxiliary functionality and types
-- Some type aliases to improve readability
type Bid = Double
type PlayerChoice = String
type Transfer = Double
type Transfer1 = Transfer
type Transfer2 = Transfer
type Agent1 = Agent
type Agent2 = Agent

-- Include dependency on players' roles into the game choice definition
gameChoice :: (Agent1,(Agent1,Agent2)) -> [Either (Agent1,Agent2) ()]
gameChoice (_,agents) =
  [Left agents, pdChoice]

-- Determines allocation of payoffs
payoffsBidding :: Agent -> Agent -> (Bid,Bid,PlayerChoice) -> (Transfer1,Transfer2,Bid,Agent1,Agent2)
payoffsBidding agentA agentB (bidAgentA, bidAgentB, choicePlayerToCommit) =
  if choicePlayerToCommit == agentA
     then (-bidAgentA,0,bidAgentA,agentA,agentB)
     else (0,-bidAgentB,bidAgentB,agentB,agentA)

--------------------
-- 2. Representation
-- 2.0. Game setup
prisonersDilemmaRoleDependency commitment = [opengame|

   inputs    : firstAgent,secondAgent ;
   feedback  :      ;

   :----------------------------:
   inputs    : secondAgent,empty  ;
   feedback  :      ;
   operation : dependentRoleDecision (const [Cooperate,Defect]);
   outputs   : decisionCooperate ;
   returns   : payoffSecondAgent - decisionTransfer;

   inputs    : secondAgent,decisionCooperate ;
   feedback  :      ;
   operation : dependentRoleDecision (const [0,1,2,3]);
   outputs   : decisionTransfer ;
   returns   : payoffSecondAgent - decisionTransfer;
   // Second's agent strategy is conditional on his cooperate decision before

   inputs    : (decisionCooperate,decisionTransfer) ;
   feedback  :      ;
   operation : forwardFunction $ commitment ;
   outputs   : decisionFirstAgent ;
   returns   : ;
   // Frist strategic choice is substituted by a computation

   inputs    : decisionFirstAgent, decisionCooperate ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ prisonersDilemmaMatrix ;
   outputs   : (payoffFirstAgent,payoffSecondAgent);
   returns   : ;

   inputs    : firstAgent,payoffFirstAgent  ;
   feedback  : ;
   operation : addRolePayoffs ;
   outputs   : ;
   returns   : ;
   // We are doing book-keeping for the first agent in case we want to embedd that game into a larger component

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- Combined branching game
-- NOTE this is a branching game; only one of the possible branches will be played
branchingGameCommitment commitment = (prisonersDilemmaRoleDependency commitment) +++ prisonersDilemmaGame

-- Choice on branching (which game to play)
-- NOTE in case of commitment game we forward the relevant role information
prisonersDilemmaAliceChoiceRoleDependency commitment = [opengame|

   inputs    : firstAgent,secondAgent  ;
   feedback  :      ;

   :----------------------------:
   inputs    : firstAgent,(firstAgent,secondAgent) ;
   feedback  :      ;
   operation : dependentRoleDecision $ gameChoice ;
   outputs   : gameDecision ;
   returns   : 0 ;

   inputs    : gameDecision ;
   feedback  :      ;
   operation : branchingGameCommitment commitment;
   outputs   : discard;
   returns   : ;
   // discard the output

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- 2.1 Coordinator
-- Defines a game where to agents, e.g. Alice and Bob, bid for to the coordinator to be the ones setting the commitment
coordinator agentA agentB payoffBidding = [opengame|

   inputs    :   ;
   feedback  :   ;

   :----------------------------:
   inputs    :  ;
   feedback  :      ;
   operation : dependentDecision agentA $ const [0,1,2,3]  ;
   outputs   : bidAgentA ;
   returns   : costsA ;
   // Agent 1 can bid on being the first to set the commitment

   inputs    :  ;
   feedback  :      ;
   operation : dependentDecision agentB $ const [0,1,2,3]  ;
   outputs   : bidAgentB ;
   returns   : costsB ;
   // Agent 2 can bid on being the first to set the commitment

   inputs    : (agentA,bidAgentA),(agentB,bidAgentB) ;
   feedback  :      ;
   operation : dependentDecision "coordinator" $ const [agentA,agentB] ;
   outputs   : choicePlayerToCommit;
   returns   : winningBid;

   inputs    : bidAgentA, bidAgentB, choicePlayerToCommit ;
   feedback  :   ;
   operation : forwardFunction $ payoffBidding agentA agentB;
   outputs   : costsA,costsB,winningBid,firstAgent,secondAgent;
   returns   : ;

   :----------------------------:

   outputs   : firstAgent,secondAgent;
   returns   :      ;
  |]

-- 2.2. Connecting the games
-- NOTE we use the allocation rule defined above
-- NOTE we fix a give commitment strategy; can be changed later
coordinatorGameWithCredibleCommitments agentA agentB commitment = [opengame|

   inputs    :   ;
   feedback  :   ;

   :----------------------------:
   inputs    :  ;
   feedback  :      ;
   operation : coordinator agentA agentB payoffsBidding ;
   outputs   : firstAgent,secondAgent ;
   returns   : ;

   inputs    : firstAgent,secondAgent;
   feedback  :      ;
   operation : prisonersDilemmaAliceChoiceRoleDependency commitment ;
   outputs   : ;
   returns   : ;


   :----------------------------:

   outputs   : ;
   returns   :      ;
  |]

