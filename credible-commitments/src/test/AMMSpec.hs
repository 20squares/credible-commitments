module AMMSpec where

import AMM.AMM
import AMM.Types
import TestParameters

import qualified Dhall.Map as M
import Test.Hspec

spec :: Spec
spec = do
  ammFunctionality
  ammMapFunctionality

ammFunctionality = describe
   "test amm functionality" $ do
     it "basic functionality - 1" $ do
       shouldBe
         (swapWithAmount transaction0 contractState1)
         transactionOutcome0
     it "basic functionality - 2" $ do
       shouldBe
         (swapWithAmount transaction1 contractState1)
         transactionOutcome1
     it "basic functionality - 3" $ do
       shouldBe
         (swapWithAmount transaction2 contractState1)
         transactionOutcome2
     it "sending 0" $ do
       shouldBe
         (swapWithAmount ((Swap1 0),10) contractState1)
         (Swap0Out 0, contractState1,10)

ammMapFunctionality = describe
   "test amm functionality for sequence of txs" $ do
     it "simple sequence - 1" $ do
       shouldBe
         (mapSwapsWithAmounts (mapTransactions1, contractState1))
         (mapTransactionOutcomes1)
     it "simple sequence - 2" $ do
       shouldBe
         (mapSwapsWithAmounts (mapTransactions2, contractState1))
         (mapTransactionOutcomes2)
