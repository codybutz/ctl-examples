module Scaffold (contract) where

import Contract.Prelude

import Contract.Chain (currentSlot)
import Contract.Hashing (nativeScriptHash)
import Contract.Monad (Contract, liftedE, liftedM, liftContractM)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (NativeScript(..))
import Contract.Time (Slot (..))
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, mkTokenName)

import Data.BigInt as BigInt

import Types.BigNum as BigNum
import NativeScripts (NativeScriptHash(..))
import Plutus.Types.CurrencySymbol (scriptHashAsCurrencySymbol, currencyMPSHash)

-- This contract fails since the MPS Hash is not a Plutus Script. The `mustMintCurrency`
-- requires that a plutus script be included as a lookup, however it's possible to use
-- Native Scripts as a minting policy.
contract :: Contract () Unit
contract = do
  slot <- currentSlot
  let newSlot = pure $ BigNum.add (unwrap slot) (BigNum.fromInt 180)
  newSlot' <- liftedM "Unable to calculate new slot" newSlot
  tn <- tokenName "TheToken"
  let script = ScriptAll [
    TimelockExpiry (Slot newSlot')
  ]
  (NativeScriptHash scriptHash) <- liftedM "Unable to create native script" $ pure $ nativeScriptHash script
  let cs = scriptHashAsCurrencySymbol scriptHash
  let mph = currencyMPSHash cs

  let
    lookups :: Lookups.ScriptLookups Void
    lookups = mempty
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintCurrency mph tn (BigInt.fromInt 1)

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  awaitTxConfirmed txId

tokenName :: String -> Contract () TokenName
tokenName =
  liftContractM "Cannot make token name"
    <<< (mkTokenName <=< byteArrayFromAscii)
