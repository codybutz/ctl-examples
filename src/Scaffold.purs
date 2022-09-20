module Scaffold (contract) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftedE, liftedM, throwContractError)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime (..), getSystemStart, getEraSummaries, getTip, slotToPosixTime, to, Tip(..))
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf)

import Data.BigInt as BigInt

contract :: Contract () Unit
contract = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  now <- currentTime
  let timeValidate = wrap $ unwrap now + (unwrap sometime)
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToPubKey pkh (lovelaceValueOf $ BigInt.fromInt 2_000_000)
      <>  Constraints.mustValidateIn (to timeValidate)
    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  awaitTxConfirmed txId

sometime :: POSIXTime
sometime = POSIXTime $ BigInt.fromInt 180_000

currentTime
  :: forall (r :: Row Type)
   . Contract r POSIXTime
currentTime = do
  currentSlot <-
    getTip >>= case _ of
      TipAtGenesis -> throwContractError "currentTime: chain tip at genesis"
      Tip tip -> pure $ (unwrap tip).slot
  es <- getEraSummaries
  ss <- getSystemStart
  liftedE $ liftEffect $ slotToPosixTime es ss currentSlot