{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.AMM where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import Data.List (permutations)

{-
Provides basic functionality for an AMM and the open game components
-}


{-
TODO
- We probably should start with 2 transactions only to keep it as close as possible to the PD
- These transactions should be identical (this should feature the PD)
- We assume that only the first transaction pays a reward to the coordinator
- What choices should the players make?
   - Choose from a subset of transactions; maybe two transactions each? Again, similar to the PD
   - Choose bid for coordinator
-}

-----------
-- 1. Types
type Fee = Double

type ContractState = (Double, Double)

type PlayerID = String

data SwapTransaction = Swap0 Double | Swap1 Double
  deriving (Show, Ord, Eq)
data Result = Swap0Out () | Swap1Out ()
  deriving (Show, Ord, Eq)

type Transaction = (SwapTransaction, Fee, PlayerID)


-------------------------
-- 2. Auxiliary functions

inRange :: Double -> Double -> Bool
inRange _ _ = True

-- Swap a given amount of tokens
swapWithAmount :: Transaction -> ContractState -> (Result, ContractState, Fee, PlayerID)
swapWithAmount ((Swap0 amt), fee, id) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
          (Swap0Out (), (reserve0 + amt
          ,(reserve0 * reserve1) / (reserve0 + amt) + 1)
          , fee
          , id)
      else (Swap0Out (), st, fee, id) -- ^ The fee is paid anyways? 
swapWithAmount ((Swap1 amt), fee, id) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
    (Swap1Out (), ((reserve0 * reserve1) / (reserve1 + amt) + 1
    ,reserve1 + amt)
    ,fee
    , id)
      else (Swap1Out (), st, fee, id) -- ^ The fee is paid anyways?

-- Thread through a sequence of transactions
-- NOTE: We assume that the first element in the list is the first transaction to be run.
-- NOTE: The latest element in the outcomes list is the latest state.
lsSwapsWithAmounts :: ([Transaction], ContractState) -> [(Result,ContractState, Fee, PlayerID)]
lsSwapsWithAmounts ([], _)                = []
lsSeapsWithAmounts ((x:xs), state) =
  let (result1,stateNew, fee, id) = swapWithAmount x state
       in (result1,stateNew, fee, id) : lsSwapsWithAmounts (xs,stateNew)

-- Create list of all possible transaction orderings
actionSpaceCoordinator :: ([Transaction],ContractState) -> [[Transaction]]
actionSpaceCoordinator (lsTransaction, _ ) = permutations lsTransaction

-- Compute payoff for coordinator
-- NOTE We assume here that only the first transaction has to pay a fee
computePayoffCoordinator :: [(Result,ContractState, Fee, PlayerID)] -> (Fee, (PlayerID,Fee))
computePayoffCoordinator ls =
  let (_,_,fee,playerID) = head ls
      in (fee,(playerID, -fee))

----------------
-- 3. Open games

-- NOTE: We assume for now that the builder is not including own transactions
coordinator  = [opengame|
  inputs: transactionsSubmitted, state ;
  feedback: ;

  :------:

  inputs :     transactionsSubmitted, state ;
  operation :  dependentDecision "coordinator" $ actionSpaceCoordinator  ;
  // We initiate the function with a zero fee ;
  outputs :    lsTransactionOrdered;
  returns :    payoffCoordinator ;


  :------:
  outputs : lsTransactionOrdered;
  returns : payoffCoordinator;
|]


amm  = [opengame|
  inputs: lsTransaction, state ;
  feedback: ;

  :------:

  inputs : lsTransaction, state ;
  operation : forwardFunction $  lsSwapsWithAmounts ;
  outputs : lsOutput;

  :------:
  outputs : lsOutput;
  returns : ;
|]


payoffs = [opengame|
  inputs: lsOutput ;
  feedback: ;

  :------:

  inputs : lsOutput ;
  operation : forwardFunction $ computePayoffCoordinator ;
  outputs : payoffCoordinator, payoffPlayer ;

  inputs :  payoffPlayer ;
  operation : addRolePayoffs ;
  outputs : ;
  
  :------:
  outputs : payoffCoordinator ;
  returns : ;
|]

{--
chooseTransactions fee1P1 fee2P1 fee1P2 fee2P2 =
  [opengame|
  inputs: state ;
  feedback: ;

  :------:

  inputs :  state ;
  operation : dependentDecision "
  outputs : lsTransactionOrdered;
  returns : payoffCoordinator;

  inputs : lsTransactionOrdered, state ;
  operation : amm ;
  outputs : lsOutput;

  inputs : lsOutput ;
  operation : payoffs ; 
  outputs : payoffCoordinator ;

  :------:
  outputs :  ;
  returns : ;

|]
-}

completeGame = [opengame|
  inputs: transactionsSubmitted, state ;
  feedback: ;

  :------:

  inputs : transactionsSubmitted, state ;
  operation : coordinator ; 
  outputs : lsTransactionOrdered;
  returns : payoffCoordinator;

  inputs : lsTransactionOrdered, state ;
  operation : amm ;
  outputs : lsOutput;

  inputs : lsOutput ;
  operation : payoffs ; 
  outputs : payoffCoordinator ;

  :------:
  outputs :  ;
  returns : ;

|]
