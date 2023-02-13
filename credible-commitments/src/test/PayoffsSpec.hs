module PayoffsSpec where

import AMM.Payoffs
import AMM.Types
import TestParameters

import Test.Hspec

spec :: Spec
spec = do
  balance
  privateValueAddedCorrectly
  balanceWPrivateValues
  computePayoffMap
  computePayoffMapWPrivateValues
  computePayoffCoordinatorFee
  computePayoffCoordinatorSum
  
-- Internal function taken for explicit testing purposes
updateBalanceTest contractState (swapTransaction,_) (result,_,_) privateValue (endowment0,endowment1)  =
  let newBalance =
        case swapTransaction of
          Swap0 sent0 ->
            case result of
              Swap0Out received0 -> (endowment0 - sent0 + received0 + privateValue, endowment1) -- ^ case should not happen
              Swap1Out received1 -> (endowment0 - sent0 + privateValue, endowment1 + received1)
          Swap1 sent1 ->
            case result of
              Swap0Out received0 -> (endowment0 + received0 + privateValue, endowment1 - sent1)
              Swap1Out received1 -> (endowment0 + privateValue, endowment1 - sent1 + received1)  -- ^ case should not happen
        in newBalance

-- Internal component for testing
findValue [] _ = 0
findValue ((name,_):xs) playerName =
      if name == playerName
        then 1
        else 1 + findValue xs playerName



balance = describe
   "test that the balance is computed correctly - no private values" $ do
     it "correct balance primitive -1" $ do
       shouldBe
         (updateBalanceTest contractState1 transaction2 transactionOutcome2 privateValue1 endowment1)
         (119,75)
     it "correct balance operation - 1" $ do
       shouldBe
         (updateBalance contractState1 transaction2 transactionOutcome2 privateValue1 endowment1)
        utility1
     it "correct balance operation with artificially different exchange rate - 1" $ do
       shouldBe
         (updateBalance contractState2 transaction2 transactionOutcome2 privateValue1 endowment1)
         utility2
     it "correct balance primitive -2" $ do
       shouldBe
         (updateBalanceTest contractState2 transaction0 transactionOutcome0 privateValue1 endowment1)
         (0,149)
     it "correct balance operation -2" $ do
       shouldBe
         (updateBalance contractState2 transaction0 transactionOutcome0 privateValue1 endowment1)
         utility3
     it "correct balance operation with artificially different exchange rate -2" $ do
       shouldBe
         (updateBalance contractState1 transaction0 transactionOutcome0 privateValue1 endowment1)
         utility4

privateValueAddedCorrectly = describe
   "test that the private value is added correctly" $ do
     it "find value correct - 1" $ do
        shouldBe
          (findValue lsTransactions1 player1)
          1
     it "find value correct - 2" $ do
        shouldBe
          (findValue lsTransactions1 player2)
          2
     it "value correct - 1" $ do
        shouldBe
          (addPrivateValue player1 lsTransactions1 lsPrivateValues1)
          0
     it "value correct - 2" $ do
        shouldBe
          (addPrivateValue player2 lsTransactions1' lsPrivateValues2)
          20

balanceWPrivateValues = describe
   "test that the balance is computed correctly - private values" $ do
     it "correct balance primitive -1" $ do
       shouldBe
         (updateBalanceTest contractState1 transaction2 transactionOutcome2 privateValue2 endowment1)
         (139,75)
     it "correct balance operation - 1" $ do
       shouldBe
         (updateBalance contractState1 transaction2 transactionOutcome2 privateValue2 endowment1)
         utilityWPrivateValue1
     it "correct balance operation with artificially different exchange rate - 1" $ do
       shouldBe
         (updateBalance contractState2 transaction2 transactionOutcome2 privateValue2 endowment1)
         utilityWPrivateValue2
     it "correct balance primitive -2" $ do
       shouldBe
         (updateBalanceTest contractState2 transaction0 transactionOutcome0 privateValue2 endowment1)
         (20,149)
     it "correct balance operation -2" $ do
       shouldBe
         (updateBalance contractState2 transaction0 transactionOutcome0 privateValue2 endowment1)
         utilityWPrivateValue3
     it "correct balance operation with artificially different exchange rate -2" $ do
       shouldBe
         (updateBalance contractState1 transaction0 transactionOutcome0 privateValue2 endowment1)
         utilityWPrivateValue4



computePayoffMap = describe
   "test utility is computed correctly for all players" $ do
     it "correct map - 1" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,lsTransactions2,lsTransactionOutcomes2,lsPrivateValues1))
         mapUtility1
     it "correct map - 2" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,lsTransactions1,lsTransactionOutcomes1,lsPrivateValues1))
         mapUtility2

computePayoffMapWPrivateValues = describe
   "test utility is computed correctly for all players" $ do
     it "correct map - 1" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,lsTransactions2,lsTransactionOutcomes2,lsPrivateValues2))
         mapUtilityWPrivateValue1
     it "correct map - 2" $ do
       shouldBe
         (computePayoffPlayerMap contractState1 (mapEndowments1,lsTransactions1,lsTransactionOutcomes1,lsPrivateValues3))
         mapUtilityWPrivateValue2



computePayoffCoordinatorFee = describe
    "payoff is computed correctely for coordinator" $ do
      it "high and low fee -1" $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (lsTransactionOutcomes1,mapUtility1))
          (10,(player1,-10))
      it "high and low fee -2" $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (lsTransactionOutcomes3,mapUtility1))
          (5,(player1,-5))
      it "low and high fee " $ do
        shouldBe
          (computePayoffCoordinatorMaxFee (lsTransactionOutcomes4,mapUtility1))
          (0,(player1,0))

computePayoffCoordinatorSum = describe
    "payoff is computed correctely for coordinator" $ do
      it "sum of utility -1" $ do
        shouldBe
          (computePayoffCoordinatorMaxPlayerUtility (lsTransactionOutcomes1,mapUtility1))
          (381.5,(player1,-10))
      it "sum of utility -2" $ do
        shouldBe
          (computePayoffCoordinatorMaxPlayerUtility (lsTransactionOutcomes2,mapUtility2))
          (265,(player1,-10))
