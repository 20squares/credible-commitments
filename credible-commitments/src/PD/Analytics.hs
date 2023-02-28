module PD.Analytics where

import OpenGames.Engine.Engine
import PD.PD
import PD.Coordinator



-----------------------
-- Equilibrium Analysis
-----------------------

-- 1. Prisoner's dilemma
isEquilibriumPrisonersDilemma strat = generateIsEq $ evaluate prisonersDilemmaGame strat void

{- Example usage:
isEquilibriumPrisonersDilemma strategyTupleDefect
-}

-- 2. Commitment
isEquilibriumPrisonersDilemmaCommitment commitment strat = generateIsEq $ evaluate (prisonersDilemmaBobUnderCommitment commitment) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaCommitment conditionalCooperate bobStrategyCooperate
-}

-- 3. Branching game
isEquilibriumPrisonersDilemmaAliceChoice commitment strat = generateIsEq $ evaluate (prisonersDilemmaAliceChoice commitment) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaAliceChoice conditionalCooperate strategyTupleCommit
-}

-- 4. Branching game with transfer
isEquilibriumPrisonersDilemmaAliceChoiceTransfer commitment strat = generateIsEq $ evaluate (prisonersDilemmaAliceChoiceTransfer commitment) strat void

-- 4.1 Branching game with transfer + bribe
isEquilibriumPrisonersDilemmaAliceChoiceTransferBribe commitment matrix cooperateValue defectValue strat = generateIsEq $ evaluate (prisonersDilemmaAliceChoiceTransferBribe commitment matrix cooperateValue defectValue) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaAliceChoiceTransfer conditionalCooperateTransfer strategyTupleCommitTransfer
-}

-- 5. Coordinator game 
isEquilibriumPrisonersDilemmaCoordinator commitment strat = generateIsEq $ evaluate (coordinatorGameWithCredibleCommitments "Alice" "Bob"  commitment) strat void

{- Example usage:
isEquilibriumPrisonersDilemmaCoordinator conditionalCooperateTransfer strategyTupleCoordinator
-}

