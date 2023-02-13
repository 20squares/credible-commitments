
module AMM.ActionSpaces where

import OpenGames.Engine.Engine
import AMM.Types
import Data.List (permutations)

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
actionSpaceCoordinator :: (TransactionsLS,ContractState) -> [TransactionsLS]
actionSpaceCoordinator (lsTransactions, _ ) = permutations lsTransactions

-- Combine two transactions into a list of transactions
combineTXIntoList name1 name2 (tx1,tx2) = [(name1,tx1),(name2,tx2)]

-- Combine two private valuations into a list of transactions
combinePrivateValuesIntoList name1 name2 (value1,value2) = [(name1,value1),(name2,value2)]
