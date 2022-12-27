{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Coordinator where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import PD

--------------
-- 0. Overview

{-
This module contains a model with coordinator
-}

-----------------------------
-- 1. Auxiliary functionality

-- Include dependency on players' roles into the game choice definition
gameChoice players =
  [commitmentChoiceDependency players, pdChoice]
  where
     commitmentChoiceDependency players = Left players

--------------------
-- 2. Representation
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
   inputs    : firstAgent,secondAgent     ;
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


