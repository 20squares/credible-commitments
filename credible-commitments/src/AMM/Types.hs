module AMM.Types where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import qualified Data.Map.Strict as M

{-
Types
-}


--------
-- Types
--------
type ExchangeRate = Double

type Utility = Double

type Fee = Double

type ContractState = (Double, Double)

type PlayerID = String

data SwapTransaction = Swap0 Double | Swap1 Double
  deriving (Show, Ord, Eq)

data Result = Swap0Out {g :: Double} | Swap1Out {g' :: Double}
  deriving (Show, Ord, Eq)

type Transaction = (SwapTransaction, Fee)
type TransactionResult =  (Result, ContractState, Fee)

type MapTransactions = M.Map PlayerID Transaction
type MapTransactionResults = M.Map PlayerID TransactionResult

type MapPlayerEndowment = M.Map PlayerID (Double,Double)

type MapPlayerUtility = M.Map PlayerID Utility

player1 = "Player 1"

player2 = "Player 2"

