{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis where

import OpenGames.Engine.Engine
import PD


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
-- Commitment strat
conditionalCooperate action =
  if action == Cooperate
     then Cooperate
     else Defect

-- Bob strategy
bobStrategyCooperate = cooperateStrategy ::- Nil

-- 3. Commitment
-- Alice chooses to commit
aliceStrategyCommit :: Kleisli Stochastic () (Either () ())
aliceStrategyCommit = pureAction commitmentChoice

-- Alice chooses not to commit - branch into PD
aliceStrategyPD = pureAction pdChoice

-- Aggregating into full strategy
strategyTupleCommit =
  aliceStrategyCommit    -- ^ which game does Alice choose?
  ::- cooperateStrategy  -- ^ if in the commitment game which action does Bob choose?
  ::- defectStrategy     -- ^ if in the pd game which action does Alice choose?
  ::- defectStrategy     -- ^ if in the pd game which action does Bob choose?
  ::- Nil 

-----------------------
-- Equilibrium Analysis
-----------------------

-- 1. Prisoner's dilemma
isEquilibriumPrisonersDilemma strat = generateIsEq $ evaluate prisonersDilemmaGame strat void

{- Example usage:
isEquilibriumPrisonersDilemma strategTupleDefect
-}

-- 2. Commitment
isEquilibriumPrisonersDilemmaCommitment aliceCommitment strat = generateIsEq $ evaluate (prisonersDilemmaBobUnderCommitment aliceCommitment) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaCommitment conditionalCooperate bobStrategyCooperate
-}

-- 3. Branching game
isEquilibriumPrisonersDilemmaAliceChoice aliceCommitment strat = generateIsEq $ evaluate (prisonersDilemmaAliceChoice aliceCommitment) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaAliceChoice conditionalCooperate strategyTupleCommit
-}

