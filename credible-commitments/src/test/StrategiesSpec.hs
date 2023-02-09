module StrategiesSpec where

import OpenGames.Engine.Engine

import AMM.ActionSpaces
import AMM.Strategies
import AMM.Types
import TestParameters

import Control.Arrow
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution (decons)
import Test.Hspec
  

spec :: Spec
spec = do
  actionSpaceTest
  coordinatorStrat

actionSpaceTest = describe 
   "coordinator actionSpace" $ do
     it "does  correctly construct action space - 1" $ do
       shouldBe
         (actionSpaceCoordinator (lsTransactions1,contractState1))
         [lsTransactions1,lsTransactions1']
     it "does  correctly construct action space - 2" $ do
       shouldBe
         (actionSpaceCoordinator (lsTransactionsFromParameterization,contractState1))
         [lsTransactionsFromParameterization,lsTransactionsFromParameterization']




coordinatorStrat = describe
   "coordinator strategy" $ do
     it "does react correctly wrt fees - 1" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (lsTransactions1,contractState1))
         [(lsTransactions1, 0.5),(lsTransactions1', 0.5)]
     it "does react correctly wrt fees - 2" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (lsTransactions3,contractState1))
         [(lsTransactions3, 1.0)]
     it "does react correctly wrt fees - 3" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (lsTransactions4,contractState1))
         [(lsTransactions4', 1.0)]
     it "does react correctly wrt fees - 4" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (lsTransactionsFromParameterization,contractState1))
         [(lsTransactionsFromParameterization', 1.0)]
