module StrategiesSpec where

import OpenGames.Engine.Engine

import AMM.ActionSpaces
import AMM.Strategies
import AMM.Types
import TestParameters

import Control.Arrow
import Data.List (maximumBy)
import qualified Dhall.Map as M
import Data.Ord (comparing)
import Numeric.Probability.Distribution (decons)
import Test.Hspec
  

spec :: Spec
spec = do
  actionSpaceTest
  coordinatorStrat

-- Test inner component
chooseMaximalFee :: [MapTransactions] -> MapTransactions
chooseMaximalFee lsOfMaps =
  fst $ maximumBy (comparing snd) [(x, snd . head . M.toList $ x)| x <- lsOfMaps]

actionSpaceTest = describe 
   "coordinator actionSpace" $ do
     it "does  correctly construct action space - 1" $ do
       shouldBe
         (actionSpaceCoordinator (mapTransactions1,contractState1))
         [mapTransactions1,mapTransactions1']
     it "does  correctly construct action space - 2" $ do
       shouldBe
         (actionSpaceCoordinator (mapTransactionsFromParameterization,contractState1))
         [mapTransactionsFromParameterization,mapTransactionsFromParameterization']




coordinatorStrat = describe
   "coordinator strategy" $ do
     it "does react correctly wrt fees - 1" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (mapTransactions1,contractState1))
         [(mapTransactions1', 1.0)]
     it "does react correctly wrt fees - 2" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (mapTransactions3,contractState1))
         [(mapTransactions3, 1.0)]
     it "does react correctly wrt fees - 3" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (mapTransactions4,contractState1))
         [(mapTransactions4', 1.0)]
     it "does react correctly wrt fees - 4" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (mapTransactionsFromParameterization,contractState1))
         [(mapTransactions4', 1.0)]
