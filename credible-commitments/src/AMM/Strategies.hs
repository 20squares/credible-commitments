{-# LANGUAGE RecordWildCards #-}

module AMM.Strategies where

import OpenGames.Engine.Engine
import AMM.AMM
import AMM.Types

{-
Defines concrete strategies
-}

-------------
-- Strategies
-------------

-- Strategy 1
strategyP1 fee Parameters{..} = pureAction (Swap0 50, fee, name1)

-- Strategy 2
strategyP2 fee Parameters{..} = pureAction (Swap0 50, fee, name2)

strategyTuple fee1 fee2 parameters = strategyP1 fee1 parameters ::- strategyP2 fee2 parameters ::- Nil
