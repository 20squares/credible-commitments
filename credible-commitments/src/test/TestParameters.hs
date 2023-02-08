module TestParameters where

import AMM.Types

import qualified Data.Map.Strict as M
-------------
-- Parameters

player1 = "player1"

player2 = "player2"

contractState1,contractState2 :: ContractState 
contractState1 = (100,100)
contractState2 = (100,50)

-- endowment
endowment1 :: Endowment
endowment1 = (100,100)

mapEndowments1 = M.fromList [(player1,contractState1),(player2,contractState1)]

-- Transaction data
transaction0,transaction1,transaction2 :: Transaction
transaction0 = ((Swap0 100),10)
transaction1 = ((Swap1 100), 10)
transaction2 = ((Swap1 25), 10)


mapTransactions1 = M.fromList [(player1,transaction1),(player2,transaction1)]
mapTransactions2 = M.fromList [(player1,transaction2),(player2,transaction2)]

-- Outcome data
transactionOutcome0, transactionOutcome1,transactionOutcome2, transactionOutcome3, transactionOutcome4, transactionOutcome5, transactionOutcome6, transactionOutcome7 :: TransactionResult
transactionOutcome0 = (Swap1Out 49, (200,51),10)
transactionOutcome1 = (Swap0Out 49, (51,200),10)
transactionOutcome2 = (Swap0Out 19, (81,125),10)
transactionOutcome3 = (Swap0Out 49, (51,200),10)
transactionOutcome4 = (Swap0Out 16, (35,300),10)
transactionOutcome5 = (Swap0Out 12.5, (68.5,150),10)
transactionOutcome6 = (Swap0Out 49, (51,200),5)
transactionOutcome7 = (Swap0Out 49, (51,200),0)
  
mapTransactionOutcomes1 = M.fromList [(player1,transactionOutcome1),(player2,transactionOutcome4)]
mapTransactionOutcomes2 = M.fromList [(player1,transactionOutcome2),(player2,transactionOutcome5)]

-- | for testing coordinator
mapTransactionOutcomes3 = M.fromList [(player1,transactionOutcome6),(player2,transactionOutcome7)]
mapTransactionOutcomes4 = M.fromList [(player1,transactionOutcome7),(player2,transactionOutcome6)]



-- Utility data
utility1,utility2,utility3,utility4,utility5, utility6 :: Double
utility1 = 194
utility2 = 156.5
utility3 = 74.5
utility4 = 149
utility5 = 187.5
utility6 = 116

mapUtility1 = M.fromList [(player1,utility1),(player2,utility5)]
mapUtility2 = M.fromList [(player1,utility4),(player2,utility6)]
