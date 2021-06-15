{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : ShelleyStyleWitness
-- Description : Almost entirely copy/paste from 'Shelley.Spec.Ledger.STS.Utxow'
module Cardano.Ledger.Voltaire.Prototype.Rules.Utxow.ShelleyStyleWitness
( ShelleyStyleWitnessNeeds,
  shelleyStyleWitness
)
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), TxInBlock)
import Shelley.Spec.Ledger.STS.Utxo
    ( UtxoEnv(..) )
import Cardano.Ledger.Voltaire.Prototype.TxBody ()
import Cardano.Ledger.Voltaire.Prototype.Rules.LedgerState (witsVKeyNeeded)
import Control.State.Transition.Extended
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.LedgerState
    ( UTxOState,
      UTxOState(..),
      WitHashes(..),
      diffWitHashes,
      nullWitHashes,
      verifiedWits,
      witsFromTxWitnesses,
      AccountState,
    )
import Shelley.Spec.Ledger.STS.Utxow
  ( UtxowPredicateFailure (..),
  )
import Shelley.Spec.Ledger.Tx
    ( ValidateScript, WitVKey, hashScript, validateScript )
import Cardano.Ledger.AuxiliaryData
  ( ValidateAuxiliaryData (..),
    hashAuxiliaryData,
  )
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (∩))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField, getField)
import Shelley.Spec.Ledger.Address (Addr)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Delegation.Certificates (isInstantaneousRewards)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.SoftForks as SoftForks
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody, TxIn, Wdrl)
import Shelley.Spec.Ledger.UTxO (scriptsNeeded)
import Cardano.Ledger.Voltaire.Prototype.Class (Update, VoltaireClass)

-- | NB: Only difference between this and
--   'Shelley.Spec.Ledger.STS.Utxow.ShelleyStyleWitnessNeeds'
--   is a different 'Update' type for @HasField "update"@.
type ShelleyStyleWitnessNeeds era =
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "addrWits" (TxInBlock era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "bootWits" (TxInBlock era) (Set (BootstrapWitness (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "address" (Core.TxOut era) (Addr (Crypto era)),
    ValidateAuxiliaryData era (Crypto era),
    ValidateScript era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  )

-- | NB: Only difference between this and
--  'Shelley.Spec.Ledger.STS.Utxow.shelleyStyleWitness'
--  is that the above 'ShelleyStyleWitnessNeeds' is used as a constraint and
-- 'Cardano.Ledger.Voltaire.Prototype.Rules.LedgerState.witsVKeyNeeded'
-- is used instead of 'Shelley.Spec.Ledger.LedgerState.witsVKeyNeeded'
--
shelleyStyleWitness ::
  forall era utxow.
  ( Era era,
    VoltaireClass era,
    BaseM (utxow era) ~ ShelleyBase,
    Embed (Core.EraRule "UTXO" era) (utxow era),
    Environment (Core.EraRule "UTXO" era) ~ (UtxoEnv era, AccountState),
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ TxInBlock era,
    Environment (utxow era) ~ (UtxoEnv era, AccountState),
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ TxInBlock era,
    -- PredicateFailure (utxow era) ~ UtxowPredicateFailure era,
    STS (utxow era),
    ShelleyStyleWitnessNeeds era
  ) =>
  (UtxowPredicateFailure era -> PredicateFailure (utxow era)) ->
  TransitionRule (utxow era)
shelleyStyleWitness embed = do
  (TRC ((utxoEnv@(UtxoEnv slot pp stakepools genDelegs), acct), u, tx)) <- judgmentContext
  let txbody = getField @"body" tx
      utxo = _utxo u
      witsKeyHashes = witsFromTxWitnesses @era tx
      auxdata = getField @"auxiliaryData" tx

  -- check scripts
  let failedScripts =
        filter
          ( \(hs, validator) ->
              hashScript @era validator /= hs
                || not (validateScript @era validator tx)
          )
          (Map.toList (getField @"scriptWits" tx))
  case failedScripts of
    [] -> pure ()
    fs -> failBecause $ embed $ ScriptWitnessNotValidatingUTXOW $ Set.fromList $ fmap fst fs

  let sNeeded = scriptsNeeded utxo tx
      sReceived = Map.keysSet (getField @"scriptWits" tx)
  sNeeded == sReceived
    ?! embed (MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived))

  -- check VKey witnesses
  verifiedWits @era tx ?!: (embed . InvalidWitnessesUTXOW)

  let needed = witsVKeyNeeded @era utxo tx utxoEnv
      missingWitnesses = diffWitHashes needed witsKeyHashes
      haveNeededWitnesses = if nullWitHashes missingWitnesses then Right () else Left missingWitnesses
  haveNeededWitnesses ?!: (embed . MissingVKeyWitnessesUTXOW)

  -- check metadata hash
  case (getField @"adHash" txbody, auxdata) of
    (SNothing, SNothing) -> pure ()
    (SJust mdh, SNothing) -> failBecause $ embed (MissingTxMetadata mdh)
    (SNothing, SJust md') ->
      failBecause $
        embed (MissingTxBodyMetadataHash (hashAuxiliaryData @era md'))
    (SJust mdh, SJust md') -> do
      hashAuxiliaryData @era md' == mdh
        ?! embed (ConflictingMetadataHash mdh (hashAuxiliaryData @era md'))

      -- check metadata value sizes
      when (SoftForks.validMetadata pp) $
        validateAuxiliaryData @era md' ?! embed InvalidMetadata

  -- check genesis keys signatures for instantaneous rewards certificates
  let genDelegates =
        Set.fromList $
          asWitness . genDelegKeyHash
            <$> Map.elems genMapping
      (WitHashes khAsSet) = witsKeyHashes
      genSig = eval (genDelegates ∩ khAsSet)
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ getField @"certs" txbody
      GenDelegs genMapping = genDelegs

  coreNodeQuorum <- liftSTS $ asks quorum
  ( not (null mirCerts)
      ==> Set.size genSig >= fromIntegral coreNodeQuorum
    )
    ?! embed (MIRInsufficientGenesisSigsUTXOW genSig)

  trans @(Core.EraRule "UTXO" era) $
    TRC ((UtxoEnv slot pp stakepools genDelegs, acct), u, tx)
