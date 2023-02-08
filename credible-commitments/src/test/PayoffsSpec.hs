module PayoffsSpec where

import AMM.Payoffs
import AMM.Types
import TestParameters

import Test.Hspec

spec :: Spec
spec = do
  balance
  computePayoffMap
  computePayoffCoordinatorFee
  computePayoffCoordinatorSum
  
-- Internal function taken for explicit testing purposes
updateBalanceTest contractState (swapTransaction,_) (result,_,_) (endowment0,endowment1) =
  let newBalance =
        case swapTransaction of
          Swap0 sent0 ->
            case result of
              Swap0Out received0 -> (endowment0 - sent0 + received0, endowment1) -- ^ case should not happen
              Swap1Out received1 -> (endowment0 - sent0, endowment1 + received1)
          Swap1 sent1 ->
            case result of
              Swap0Out received0 -> (endowment0 + received0, endowment1 - sent1)
              Swap1Out received1 -> (endowment0, endowment1 - sent1 + received1)  -- ^ case should not happen
        in newBalance


balance = describe
   "test that the balance is computed correctly" $ do
     it "correct balance primitive -1" $ do
       shouldBe
         (updateBalanceTest contractState1 transaction2 transactionOutcome2 endowment1)
         (119,75)
     it "correct balance operation - 1" $ do
       shouldBe
         (updateBalance contractState1 transaction2 transactionOutcome2 endowment1)
        utility1
     it "correct balance operation with artificially different exchange rate - 1" $ do
       shouldBe
         (updateBalance contractState2 transaction2 transactionOutcome2 endowment1)
         utility2
     it "correct balance primitive -2" $ do
       shouldBe
         (updateBalanceTest contractState2 transaction0 transactionOutcome0 endowment1)
         (0,149)
     it "correct balance operation -2" $ do
       shouldBe
         (updateBalance contractState2 transaction0 transactionOutcome0 endowment1)
         utility3
     it "correct balance operation with artificially different exchange rate -2" $ do
       shouldBe
         (updateBalance contractState1 transaction0 transactionOutcome0 endowment1)
         utility4

computePayoffMap = describe
   "test utility is computed correctly for all players" $ do
     it "correct map - 1" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,mapTransactions2,mapTransactionOutcomes2))
         mapUtility1
     it "correct map - 2" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,mapTransactions1,mapTransactionOutcomes1))
         mapUtility2


computePayoffCoordinatorFee = describe
    "payoff is computed correctely for coordinator" $ do
      it "high and low fee -1" $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (mapTransactionOutcomes1,mapUtility1))
          (10,(player1,-10))
      it "high and low fee -2" $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (mapTransactionOutcomes3,mapUtility1))
          (5,(player1,-5))
      it "low and high fee " $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (mapTransactionOutcomes4,mapUtility1))
          (0,(player1,0))

computePayoffCoordinatorSum = describe
    "payoff is computed correctely for coordinator" $ do
      it "sum of utility -1" $ do
        shouldBe
          (computePayoffCoordinatorMaxPlayerUtility (mapTransactionOutcomes1,mapUtility1))
          (381.5,(player1,-10))
      it "sum of utility -2" $ do
        shouldBe
          (computePayoffCoordinatorMaxPlayerUtility (mapTransactionOutcomes2,mapUtility2))
          (265,(player1,-10))
