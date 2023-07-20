{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad        (replicateM, unless)
import           Plutus.Model
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty
import qualified Permanentum            as P


type Homework1Script = TypedValidator () (Bool, Bool)

script1 :: Homework1Script
script1 = TypedValidator $ toV2 P.validator

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Permanentum tests"
      [ 
        homework1 defaultBabbage
      ]

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Validator: Basic runs"
    [ good "Case: (True, True)" $ runWith (True, True)
    , bad "Case: (True, False)" $ runWith (True, False)
    , bad "Case: (False, True)" $ runWith (False, True)
    , bad "Case: (False, False)" $ runWith (False, False)
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

runWith :: (Bool, Bool) -> Run ()
runWith redeemer = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = adaValue 100
  checkBalance (gives u1 val script1) $ do
    sp <- spend u1 val
    submitTx u1 $ giveTx sp val

  utxos <- utxoAt script1
  let [(giftRef, giftOut)] = utxos
  checkBalance (gives script1 (txOutValue giftOut) u2) $ do
    submitTx u2 $ takeTx u2 redeemer giftRef (txOutValue giftOut)
  
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Final balances are incorrect"

giveTx :: UserSpend -> Value -> Tx
giveTx usp val = 
  mconcat
    [ userSpend usp
    , payToScript script1 (HashDatum ()) val
    ]

takeTx :: PubKeyHash -> (Bool, Bool) -> TxOutRef -> Value -> Tx
takeTx pkh redeemer giftRef giftVal =
  mconcat
    [ spendScript script1 giftRef redeemer ()
    , payToKey pkh giftVal
    ]
