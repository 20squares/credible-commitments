
module AMM.ActionSpaces where

import OpenGames.Engine.Engine
import AMM.Types
import Data.List (permutations)
import qualified Data.HashMap.Strict as M

{-
Defines the available actions for players
-}

----------------
-- Action spaces
----------------

-- Define actions space for fee
actionSpaceFee upperBound  = [0..upperBound]

-- Combine choices into full tx format
combineTXAndFee swap fee = (swap, fee)

-- Create list of all possible transaction orderings
actionSpaceCoordinator :: (MapTransactions,ContractState) -> [MapTransactions]
actionSpaceCoordinator (mapTransactions, _ ) =
  let ls = M.toList mapTransactions
      in map M.fromList (permutations ls)

-- Combine two transactions into a list of transactions
combineTXIntoList name1 name2 (tx1,tx2) = M.fromList [(name1,tx1),(name2,tx2)]


-- FIXME
-- Transaction data
player1 = "player1"

player2 = "player2"


transaction0,transaction1,transaction2,transaction3,transaction4 :: Transaction
transaction0 = ((Swap0 100),10)
transaction1 = ((Swap1 100), 10)
transaction2 = ((Swap1 25), 10)
transaction3 = ((Swap1 25), 5)
transaction4 = ((Swap1 25), 4)

mapTransactions1 = M.fromList [(player1,transaction1),(player2,transaction1)]
mapTransactions2 = M.fromList [(player1,transaction2),(player2,transaction2)]
mapTransactions3 = M.fromList [(player1,transaction3),(player2,transaction4)]
mapTransactions4 = M.fromList [(player2,transaction4),(player1,transaction3)]

