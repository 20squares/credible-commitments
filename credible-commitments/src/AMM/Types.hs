module AMM.Types where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import qualified Data.HashMap.Strict as M

{-
Types
-}


--------
-- Types
--------
type ExchangeRate = Double

type Utility = Double

type Fee = Double

type Endowment = (Double, Double)

type CoordinatorPayoff = Double

type ContractState = (Double, Double)

type PlayerID = String

data SwapTransaction = Swap0 Double | Swap1 Double
  deriving (Show, Ord, Eq)

data Result = Swap0Out {g :: Double} | Swap1Out {g' :: Double}
  deriving (Show, Ord, Eq)

type Transaction = (SwapTransaction, Fee)
type TransactionResult =  (Result, ContractState, Fee)

type MapTransactions = M.HashMap PlayerID Transaction
type MapTransactionResults = M.HashMap PlayerID TransactionResult

type MapPlayerEndowment = M.HashMap PlayerID (Double,Double)

type MapPlayerUtility = M.HashMap PlayerID Utility

data Parameters = Parameters
  { exchangeRate :: ContractState
  , name1        :: Agent
  , name2        :: Agent
  , upperBound   :: Double
  , actionSpaceTXs1 :: [SwapTransaction]
  , actionSpaceTXs2 :: [SwapTransaction]
  , goalFunctionCoordinator :: (MapTransactionResults, MapPlayerUtility) -> (CoordinatorPayoff, (PlayerID,Fee))
  , mapEndowments :: MapPlayerEndowment
  } 
