module StrategiesSpec where

import OpenGames.Engine.Engine

import AMM.Strategies
import AMM.Types
import TestParameters

import Control.Arrow
import Numeric.Probability.Distribution (decons)
import Test.Hspec
  

spec :: Spec
spec = do
  coordinatorStrat

coordinatorStrat = describe
   "coordinator strategy" $ do
     it "does react correctly wrt fees" $ do
       shouldBe
         (decons $ runKleisli maxFeeStrategy $ (mapTransactions1,contractState1))
         [(mapTransactions1, 1.0)]
