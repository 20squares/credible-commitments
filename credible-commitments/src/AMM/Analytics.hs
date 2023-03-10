{-# LANGUAGE RecordWildCards #-}

module AMM.Analytics where

import OpenGames.Engine.Engine
import AMM.AMM
import AMM.Model
import AMM.Types

{-
Defines concrete strategies
-}

------------------------
-- 1. Equilibrium notion
------------------------
-- | Equilibrium definition for complete game
equilibriumCompleteGame strategy Parameters{..} = evaluate (completeGame exchangeRate name1 name2 upperBound goalFunctionCoordinator actionSpaceTXs1 actionSpaceTXs2) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(mapEndowments,exchangeRate))) (\_ _ -> pure ())

------------------------------------------
-- 2. Display equilibrium information only
------------------------------------------
printEquilibriumCompleteGame strategy parameters = generateIsEq $ equilibriumCompleteGame strategy parameters

printOutputCompleteGame strategy parameters = generateOutput $ equilibriumCompleteGame strategy parameters

findEqCompleteGame strategy parameters = generateEquilibrium $ equilibriumCompleteGame strategy parameters
