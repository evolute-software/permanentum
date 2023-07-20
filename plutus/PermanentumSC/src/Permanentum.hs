{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Permanentum where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (BuiltinByteString, Integer, Bool (..), BuiltinData, Eq ((==)), (&&))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- Types ---------------------------------------------------------

type CID = BuiltinByteString -- ToDo: Opaque type with smart construction? 

data BondStatus = Open | SelectingOffers | Funding | Persisting | Live -- | Closed
unstableMakeIsData ''BondStatus

data Liveness = Premature | Stable | Degraded
unstableMakeIsData ''Liveness

data Bond = Bond 
    { cid :: CID -- the cid containing the bond's cids
    , slots :: Integer -- how many offers can parallelly be active on the bond
    , status :: BondStatus 
    , initialFunds :: PlutusV2.Value
    }
unstableMakeIsData ''Bond -- Use TH to create an instance for IsData.

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkBondValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are True!
mkBondValidator :: Bond -> (Bool, Bool) -> PlutusV2.ScriptContext -> Bool
mkBondValidator _ (rd1, rd2) _ = rd1 && rd1 == rd2 

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkBondValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])
