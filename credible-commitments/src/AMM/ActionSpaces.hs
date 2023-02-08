
module AMM.ActionSpaces where

import OpenGames.Engine.Engine
import AMM.Types
import Data.List (permutations)
import qualified Data.Map.Strict as M

{-
Defines the available actions for players
-}

----------------
-- Action spaces
----------------

-- Define action space for choosing a tx
actionSpaceTXs = [Swap0 50, Swap1 100]

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
