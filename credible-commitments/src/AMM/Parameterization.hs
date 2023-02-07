{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module AMM.Parameterization where

import AMM.Types
import qualified Data.Map.Strict as M

{-
Instantiates the basic functionality
-}

-------------------------
-- Parameters

initialExchangeBalance :: ContractState
initialExchangeBalance = (100,100)

-- TODO Check this; or do we want to include the slippage? 
initialExchangeRate2To1 = snd initialExchangeBalance / fst initialExchangeBalance

