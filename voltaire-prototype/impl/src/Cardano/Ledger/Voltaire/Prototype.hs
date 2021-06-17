{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Ledger.Voltaire.Prototype where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era, SupportsSegWit (..), ValidateScript (..), PreviousEra)
import qualified Cardano.Ledger.Mary.Value as V
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (nativeMultiSigTag, ShelleyEra)
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AuxiliaryData,
    pattern AuxiliaryData,
  )
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    validateTimelock,
  )
import Cardano.Ledger.Val (Val ((<->), coin))
import Cardano.Ledger.Voltaire.Prototype.Class
import Cardano.Ledger.Voltaire.Prototype.Rules.Utxo (UTXO)
import Cardano.Ledger.Voltaire.Prototype.Rules.Utxow (UTXOW)
import Cardano.Ledger.Voltaire.Prototype.TxBody
import qualified Cardano.Ledger.Voltaire.Prototype.One as One
import qualified Cardano.Ledger.Voltaire.Prototype.Two as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Deleg as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Upec as Two
import Control.DeepSeq (deepseq)
import Control.SetAlgebra (eval, (◁))
import qualified Control.State.Transition as S
import Data.Default.Class (def, Default)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
  ( TxSeq (..),
    txSeqTxns,
  )
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Shelley.Spec.Ledger.Metadata (validMetadatum)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import qualified Shelley.Spec.Ledger.STS.Bbody as Shelley
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Shelley.Spec.Ledger.STS.Newpp as Shelley
import qualified Shelley.Spec.Ledger.STS.Ocert as Shelley
import qualified Shelley.Spec.Ledger.STS.Overlay as Shelley
import qualified Shelley.Spec.Ledger.STS.Rupd as Shelley
import qualified Shelley.Spec.Ledger.STS.Snap as Shelley
import qualified Shelley.Spec.Ledger.STS.Tick as Shelley
import qualified Shelley.Spec.Ledger.STS.Upec as Shelley
import Shelley.Spec.Ledger.Tx (Tx, TxOut (..), WitnessSet)
import Shelley.Spec.Ledger.Keys (asWitness)

data VoltairePrototype
  = VoltairePrototype_One
  | VoltairePrototype_Two

data VoltairePrototypeEra (proto :: VoltairePrototype) c

--------------------------------------------------------------------------------
-- Voltaire instances
--------------------------------------------------------------------------------

-- One

instance (CryptoClass.Crypto c) => VoltaireClass (VoltairePrototypeEra 'VoltairePrototype_One c) where
  type ProposalHeader (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.ProposalHeader (VoltairePrototypeEra 'VoltairePrototype_One c)
  type ProposalBody (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.ProposalBody (VoltairePrototypeEra 'VoltairePrototype_One c)
  type ProposalId (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.ProposalId (VoltairePrototypeEra 'VoltairePrototype_One c)
  type PpupEnv (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.PpupEnv (VoltairePrototypeEra 'VoltairePrototype_One c)
  type PpupState (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.PpupState (VoltairePrototypeEra 'VoltairePrototype_One c)
  type PpupPredicateFailure (VoltairePrototypeEra 'VoltairePrototype_One c)
    = One.PpupPredicateFailure (VoltairePrototypeEra 'VoltairePrototype_One c)
  fromUtxoEnv = One.fromUtxoEnv
  ppupTransition = One.ppupTransition
  submissionsWitnesses (Shelley.UtxoEnv _ _ _ (Shelley.GenDelegs genDelegs)) submissions =
    Set.map asWitness . Set.fromList $ Map.elems updateKeys
   where
    submissionsKeys =
        Set.fromList
      . fmap (One.proposal_submitter . proposalHeader)
      . toList
      . submissionSeq
    updateKeys' = eval (submissionsKeys submissions ◁ genDelegs)
    updateKeys = Map.map Shelley.genDelegKeyHash updateKeys'
    submissionSeq (Submissions seq') = seq'

type instance PreviousEra (VoltairePrototypeEra 'VoltairePrototype_One c) = ShelleyEra c

-- Two

instance (CryptoClass.Crypto c) => VoltaireClass (VoltairePrototypeEra 'VoltairePrototype_Two c) where
  type ProposalHeader (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = One.ProposalHeader (VoltairePrototypeEra 'VoltairePrototype_Two c)
  type ProposalBody (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = Two.ProposalBody (VoltairePrototypeEra 'VoltairePrototype_Two c)
  type ProposalId (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = Two.ProposalId (VoltairePrototypeEra 'VoltairePrototype_Two c)
  type PpupEnv (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = One.PpupEnv (VoltairePrototypeEra 'VoltairePrototype_Two c)
  type PpupState (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = Two.PPUPState (VoltairePrototypeEra 'VoltairePrototype_Two c)
  type PpupPredicateFailure (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = One.PpupPredicateFailure (VoltairePrototypeEra 'VoltairePrototype_Two c)
  fromUtxoEnv = One.fromUtxoEnv
  ppupTransition = Two.ppupTransition
  -- NB: identical to that of VoltairePrototype_One
  submissionsWitnesses (Shelley.UtxoEnv _ _ _ (Shelley.GenDelegs genDelegs)) submissions =
    Set.map asWitness . Set.fromList $ Map.elems updateKeys
   where
    submissionsKeys =
        Set.fromList
      . fmap (One.proposal_submitter . proposalHeader)
      . toList
      . submissionSeq
    updateKeys' = eval (submissionsKeys submissions ◁ genDelegs)
    updateKeys = Map.map Shelley.genDelegKeyHash updateKeys'
    submissionSeq (Submissions seq') = seq'

type instance PreviousEra (VoltairePrototypeEra 'VoltairePrototype_Two c) =
  VoltairePrototypeEra 'VoltairePrototype_One c

--------------------------------------------------------------------------------
-- Era and Shelley instances
--------------------------------------------------------------------------------

instance
  forall c (proto :: VoltairePrototype).
  ( CryptoClass.Crypto c
  , Typeable proto
  , VoltaireClass (VoltairePrototypeEra proto c)) =>
  Era (VoltairePrototypeEra proto c) where
  type Crypto (VoltairePrototypeEra proto c) = c

instance
  forall c (proto :: VoltairePrototype).
  ( CryptoClass.Crypto c
  , Typeable proto
  , VoltaireClass (VoltairePrototypeEra proto c)) =>
  UsesValue (VoltairePrototypeEra proto c)

instance
  forall c (proto :: VoltairePrototype).
  ( CryptoClass.Crypto c
  , Typeable proto
  , VoltaireClass (VoltairePrototypeEra proto c)) =>
  UsesTxOut (VoltairePrototypeEra proto c) where
  makeTxOut _ a v = TxOut a v

instance
  ( CryptoClass.Crypto c
  , Typeable proto
  , VoltaireClass (VoltairePrototypeEra proto c)) =>
  UsesPParams (VoltairePrototypeEra proto c)
  where
  type
    PParamsDelta (VoltairePrototypeEra proto c) =
      Shelley.PParamsUpdate (VoltairePrototypeEra proto c)
  mergePPUpdates _ = Shelley.updatePParams

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (VoltairePrototypeEra m c) = V.Value c

type instance
  Core.TxOut (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    TxOut (VoltairePrototypeEra proto c)

type instance
  Core.TxBody (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    TxBody (VoltairePrototypeEra proto c)

type instance
  Core.Script (VoltairePrototypeEra (_proto :: VoltairePrototype) c) =
    Timelock c

type instance
  Core.AuxiliaryData (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    AuxiliaryData (VoltairePrototypeEra (proto :: VoltairePrototype) c)

type instance
  Core.PParams (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    Shelley.PParams (VoltairePrototypeEra (proto :: VoltairePrototype) c)

{- type instance
  Core.Tx (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    Tx (VoltairePrototypeEra (proto :: VoltairePrototype) c)
-}

type instance
  Core.Witnesses (VoltairePrototypeEra (proto :: VoltairePrototype) c) =
    WitnessSet (VoltairePrototypeEra (proto :: VoltairePrototype) c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  ( CryptoClass.Crypto c,
    UsesTxBody (VoltairePrototypeEra proto c),
    Core.AnnotatedData (Core.AuxiliaryData (VoltairePrototypeEra proto c)),
    (HasField "vldt" (Core.TxBody (VoltairePrototypeEra proto c)) ValidityInterval)
  ) =>
  ValidateScript (VoltairePrototypeEra proto c)
  where
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  validateScript script tx = validateTimelock script tx

-- Uses the default instance of hashScript

instance
  ( CryptoClass.Crypto c,
    Typeable proto,
    VoltaireClass (VoltairePrototypeEra proto c)
  ) =>
  SupportsSegWit (VoltairePrototypeEra proto c)
  where
  type TxInBlock (VoltairePrototypeEra proto c) = Tx (VoltairePrototypeEra proto c)
  type TxSeq (VoltairePrototypeEra proto c) = Shelley.TxSeq (VoltairePrototypeEra proto c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = Shelley.TxSeq
  hashTxSeq = Shelley.bbHash

instance
  ( CryptoClass.Crypto c,
    Core.AnnotatedData (Core.Script (VoltairePrototypeEra proto c))
  ) =>
  ValidateAuxiliaryData (VoltairePrototypeEra (proto :: VoltairePrototype) c) c
  where
  validateAuxiliaryData (AuxiliaryData md as) = deepseq as $ all validMetadatum md
  hashAuxiliaryData aux = AuxiliaryDataHash (hashAnnotated aux)

instance
  forall proto c.
  HasField "minted" (TxBody (VoltairePrototypeEra (proto :: VoltairePrototype) c)) (Set.Set (ScriptHash c))
  where
  getField x = Set.map V.policyID (V.policies (getField @"mint" x))

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are all inherited from Shelley

type instance Core.EraRule "BBODY" (VoltairePrototypeEra proto c) = Shelley.BBODY (VoltairePrototypeEra proto c)

type instance Core.EraRule "DELEG" (VoltairePrototypeEra 'VoltairePrototype_One c) = Shelley.DELEG (VoltairePrototypeEra 'VoltairePrototype_One c)

type instance Core.EraRule "DELEGS" (VoltairePrototypeEra proto c) = Shelley.DELEGS (VoltairePrototypeEra proto c)

type instance Core.EraRule "DELPL" (VoltairePrototypeEra proto c) = Shelley.DELPL (VoltairePrototypeEra proto c)

type instance Core.EraRule "EPOCH" (VoltairePrototypeEra proto c) = Shelley.EPOCH (VoltairePrototypeEra proto c)

type instance Core.EraRule "LEDGER" (VoltairePrototypeEra proto c) = Shelley.LEDGER (VoltairePrototypeEra proto c)

type instance Core.EraRule "LEDGERS" (VoltairePrototypeEra proto c) = Shelley.LEDGERS (VoltairePrototypeEra proto c)

type instance Core.EraRule "MIR" (VoltairePrototypeEra proto c) = Shelley.MIR (VoltairePrototypeEra proto c)

type instance Core.EraRule "NEWEPOCH" (VoltairePrototypeEra proto c) = Shelley.NEWEPOCH (VoltairePrototypeEra proto c)

type instance Core.EraRule "NEWPP" (VoltairePrototypeEra proto c) = Shelley.NEWPP (VoltairePrototypeEra proto c)

type instance Core.EraRule "OCERT" (VoltairePrototypeEra proto c) = Shelley.OCERT c

type instance Core.EraRule "OVERLAY" (VoltairePrototypeEra proto c) = Shelley.OVERLAY c

type instance Core.EraRule "POOL" (VoltairePrototypeEra proto c) = Shelley.POOL (VoltairePrototypeEra proto c)

type instance Core.EraRule "POOLREAP" (VoltairePrototypeEra proto c) = Shelley.POOLREAP (VoltairePrototypeEra proto c)

type instance Core.EraRule "RUPD" (VoltairePrototypeEra proto c) = Shelley.RUPD (VoltairePrototypeEra proto c)

type instance Core.EraRule "SNAP" (VoltairePrototypeEra proto c) = Shelley.SNAP (VoltairePrototypeEra proto c)

type instance Core.EraRule "TICK" (VoltairePrototypeEra proto c) = Shelley.TICK (VoltairePrototypeEra proto c)

type instance Core.EraRule "TICKF" (VoltairePrototypeEra proto c) = Shelley.TICKF (VoltairePrototypeEra proto c)

type instance Core.EraRule "TICKN" (VoltairePrototypeEra proto _c) = Shelley.TICKN

type instance Core.EraRule "UPEC" (VoltairePrototypeEra 'VoltairePrototype_One c)
  = Shelley.UPEC (VoltairePrototypeEra 'VoltairePrototype_One c)

-- These rules are defined anew in the voltaire prototype eras

type instance Core.EraRule "UPEC" (VoltairePrototypeEra 'VoltairePrototype_Two c)
  = Two.UPEC (VoltairePrototypeEra 'VoltairePrototype_Two c)

type instance Core.EraRule "DELEG" (VoltairePrototypeEra 'VoltairePrototype_Two c)
  = Two.DELEG (VoltairePrototypeEra 'VoltairePrototype_Two c)

type instance Core.EraRule "UTXO" (VoltairePrototypeEra proto c) = UTXO (VoltairePrototypeEra proto c)

type instance Core.EraRule "UTXOW" (VoltairePrototypeEra proto c) = UTXOW (VoltairePrototypeEra proto c)

-- TODO: Once the governance design is being formalized it is probable
-- that there will be a new set of transition rules that work differently
-- from the protocol parameter update scheme of pre-Voltaire Shelley.
type instance Core.EraRule "PPUP" (VoltairePrototypeEra proto c) = PPUP (VoltairePrototypeEra proto c)

class
  ( S.STS (Core.EraRule "BBODY" era)
  , S.STS (Core.EraRule "DELEG" era)
  , S.STS (Core.EraRule "DELEGS" era)
  , S.STS (Core.EraRule "EPOCH" era)
  , S.STS (Core.EraRule "LEDGER" era)
  , S.STS (Core.EraRule "LEDGERS" era)
  , S.STS (Core.EraRule "MIR" era)
  , S.STS (Core.EraRule "NEWEPOCH" era)
  , S.STS (Core.EraRule "NEWPP" era)
  , S.STS (Core.EraRule "OCERT" era)
  , S.STS (Core.EraRule "OVERLAY" era)
  , S.STS (Core.EraRule "POOL" era)
  , S.STS (Core.EraRule "POOLREAP" era)
  , S.STS (Core.EraRule "PPUP" era)
  , S.STS (Core.EraRule "RUPD" era)
  , S.STS (Core.EraRule "SNAP" era)
  , S.STS (Core.EraRule "TICK" era)
  , S.STS (Core.EraRule "TICKF" era)
  , S.STS (Core.EraRule "TICKN" era)
  , S.STS (Core.EraRule "UPEC" era)
  , S.STS (Core.EraRule "UTXO" era)
  , S.STS (Core.EraRule "UTXOW" era)
  )
  => AssertCoherentSTS era where

instance AssertCoherentSTS (VoltairePrototypeEra 'VoltairePrototype_One CryptoClass.StandardCrypto)
-- instance AssertCoherentSTS (VoltairePrototypeEra 'VoltairePrototype_Two CryptoClass.StandardCrypto)

instance
  ( CryptoClass.Crypto c,
    Default (Shelley.State (Core.EraRule "PPUP" (VoltairePrototypeEra proto c))),
    VoltaireClass (VoltairePrototypeEra proto c),
    Typeable proto
  ) =>
  Shelley.CanStartFromGenesis (VoltairePrototypeEra proto c) where
  initialState sg () =
    Shelley.NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( Shelley.EpochState
          (Shelley.AccountState (Shelley.Coin 0) reserves)
          emptySnapShots
          (Shelley.LedgerState
              ( Shelley.UTxOState
                  initialUtxo
                  (Shelley.Coin 0)
                  (Shelley.Coin 0)
                  def
              )
              (Shelley.DPState (def {Shelley._genDelegs = Shelley.GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (Shelley.PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = Shelley.genesisUTxO sg
      reserves =
        Shelley.word64ToCoin (Shelley.sgMaxLovelaceSupply sg)
          <-> coin (Shelley.balance initialUtxo)
      genDelegs = Shelley.sgGenDelegs sg
      pp = Shelley.sgProtocolParams sg
