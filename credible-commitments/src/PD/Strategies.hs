{-# LANGUAGE LambdaCase #-}

module PD.Strategies where

import OpenGames.Engine.Engine
import PD.PD
import PD.Coordinator
import GHC.Base (Double)


-------------
-- Strategies
-------------

-- 1. Prisoner's dilemma
-- | Define pure single player strategies
cooperateStrategy :: Kleisli Stochastic () ActionPD
cooperateStrategy = pureAction Cooperate
-- ^ play _Cooperate_ with certainty
defectStrategy :: Kleisli Stochastic () ActionPD
defectStrategy = pureAction Defect
-- ^ play _Defect_ with certainty

-- | Combine single player's strategies into a tuple
strategyTupleCooperate = cooperateStrategy ::- cooperateStrategy ::- Nil
-- ^ Both players cooperate with certainty
strategyTupleDefect = defectStrategy ::- defectStrategy ::- Nil
-- ^ Both players defect with certainty

-- 2. Commitment
-- Commitment strategy
conditionalCooperate action =
  if action == Cooperate
     then Cooperate
     else Defect

-- Commitment strategy with transfer
conditionalCooperateTransfer (action,transfer) =
  if action == Cooperate && transfer >= 1
     then Cooperate
     else Defect

-- Bob strategy
bobStrategyCooperate = cooperateStrategy ::- Nil

-- Alice chooses to commit
aliceStrategyCommit :: Kleisli Stochastic () (Either () ())
aliceStrategyCommit = pureAction commitmentChoice

-- Alice chooses not to commit - branch into PD
aliceStrategyPD = pureAction pdChoice

-- Bob transfer strategy
transferStrategy :: Kleisli Stochastic ActionPD Double 
transferStrategy =
  Kleisli $
    (\case
       Cooperate -> (playDeterministically 1)
       Defect    -> (playDeterministically 0)
    )

-- 3. Coordinator game
-- 3.1 Bidding

-- NOTE: simplified assumption regarding bidding
biddingStrategy :: Kleisli Stochastic () Double
biddingStrategy = pureAction 1

-- Strategy for the first player to commit
-- NOTE we are feeding the information for first player and second player name identifiers forward
firstPlayerStrategyCommit :: Kleisli Stochastic (Agent1,Agent2) (Either (Agent1,Agent2) ())
firstPlayerStrategyCommit =
  Kleisli 
   (\agents -> playDeterministically $ Left agents)

-- Fix strategy for coordinator to choose
-- NOTE we default to player B in case of a tie for simplicity
choosePlayerToCommit :: Kleisli Stochastic ((Agent,Bid),(Agent,Bid)) Agent1
choosePlayerToCommit =
  Kleisli
    (\((agentA,bidA),(agentB,bidB)) ->
    if bidA > bidB
        then playDeterministically agentA
        else if bidA < bidB
                then playDeterministically agentB
                else uniformDist [agentA,agentB]
    )

-- 4. Full strategy profiles
-- Aggregating into full strategy
strategyTupleCommit =
  aliceStrategyCommit    -- ^ which game does Alice choose?
  ::- cooperateStrategy  -- ^ if in the commitment game which action does Bob choose?
  ::- defectStrategy     -- ^ if in the pd game which action does Alice choose?
  ::- defectStrategy     -- ^ if in the pd game which action does Bob choose?
  ::- Nil

-- Aggregating into full strategy for commitment + transfer
strategyTupleCommitTransfer =
  aliceStrategyCommit     -- ^ which game does Alice choose?
  ::- cooperateStrategy   -- ^ if in the commitment game which action does Bob choose?
  ::- transferStrategy -- ^ if in the commitment game which transfer does Bob choose?
  ::- defectStrategy      -- ^ if in the pd game which action does Alice choose?
  ::- defectStrategy      -- ^ if in the pd game which action does Bob choose?
  ::- Nil

-- Aggregating into full strategy for commitment + transfer with coordinator
strategyTupleCoordinator =
  biddingStrategy               -- ^ bidding strategy of player A
  ::- biddingStrategy           -- ^ bidding strategy of player B
  ::- choosePlayerToCommit      -- ^ coordinator choose the player who can commit
  ::- firstPlayerStrategyCommit -- ^ which game does player A or B choose?
  ::- cooperateStrategy         -- ^ if in the commitment game which action does A choose?
  ::- transferStrategy       -- ^ if in the commitment game which transfer does B choose?
  ::- defectStrategy            -- ^ if in the pd game which action does A choose?
  ::- defectStrategy            -- ^ if in the pd game which action does B choose?
  ::- Nil
