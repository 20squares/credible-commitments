{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

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
strategyP1 fee = pureAction (Swap0 50, fee, player1)

-- Strategy 2
strategyP2 fee = pureAction (Swap0 50, fee, player2)

strategyTuple fee1 fee2 = strategyP1 fee1 ::- strategyP2 fee2 ::- Nil
