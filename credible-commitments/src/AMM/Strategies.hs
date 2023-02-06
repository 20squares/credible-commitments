{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module AMM.Strategies where

import OpenGames.Engine.Engine
import AMM


-------------
-- Strategies
-------------

player1 = "Player 1"

player2 = "Player 2"



-- Define strategy space for player1
strategies1 fee1 fee2  = [(Swap0 50, fee1, player1), (Swap1 100, fee2, player1)]

-- Define strategy space for player2
strategies2 fee1 fee2  = [(Swap0 50, fee1, player2), (Swap1 200, fee2, player2)]


-- Strategy 1
strategyP1 fee = pureAction (Swap0 50, fee, player1)

-- Strategy 2
strategyP2 fee = pureAction (Swap0 50, fee, player2)

strategyTuple fee1 fee2 = strategyP1 fee1 ::- strategyP2 fee2 ::- Nil
