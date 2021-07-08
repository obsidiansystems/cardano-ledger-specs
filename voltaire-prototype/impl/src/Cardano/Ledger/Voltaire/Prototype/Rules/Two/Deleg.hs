{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Ledger.Voltaire.Prototype.Rules.Two.Deleg
  ( DELEG,
    Shelley.DCert (..),
    DelegEnv (..),
    PredicateFailure,
    DelegPredicateFailure (..),
  )
where

import Cardano.Prelude (Word8)
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, range, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
    invalidKey,
  )
import Shelley.Spec.Ledger.Credential (Credential)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VerKeyVRF,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DState,
    FutureGenDeleg (..),
    _delegations,
    _fGenDelegs,
    _genDelegs,
    _ptrs,
    _rewards,
  )
import Shelley.Spec.Ledger.PParams (ProtVer)
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    SlotNo,
    (+*),
  )
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Shelley.Spec.Ledger.TxBody
  ( DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    Ptr
  )

data DELEG era

data DelegEnv era = DelegEnv
  { slotNo :: SlotNo,
    ptr_ :: Ptr,
    ppDE :: Core.PParams era -- The protocol parameters are only used for the HardFork mechanism
  }

deriving instance (Show (Core.PParams era)) => Show (DelegEnv era)

deriving instance (Eq (Core.PParams era)) => Eq (DelegEnv era)

-- Same as Shelley's except no MIR-related failures
data DelegPredicateFailure era
  = StakeKeyAlreadyRegisteredDELEG
      !(Credential 'Staking (Crypto era)) -- Credential which is already registered
  | -- | Indicates that the stake key is somehow already in the rewards map.
    --   This error is now redundant with StakeKeyAlreadyRegisteredDELEG.
    --   We should remove it and replace its one use with StakeKeyAlreadyRegisteredDELEG.
    StakeKeyInRewardsDELEG
      !(Credential 'Staking (Crypto era)) -- DEPRECATED, now redundant with StakeKeyAlreadyRegisteredDELEG
  | StakeKeyNotRegisteredDELEG
      !(Credential 'Staking (Crypto era)) -- Credential which is not registered
  | StakeKeyNonZeroAccountBalanceDELEG
      !(Maybe Coin) -- The remaining reward account balance, if it exists
  | StakeDelegationImpossibleDELEG
      !(Credential 'Staking (Crypto era)) -- Credential that is not registered
  | WrongCertificateTypeDELEG -- The DCertPool constructor should not be used by this transition
  | GenesisKeyNotInMappingDELEG
      !(KeyHash 'Genesis (Crypto era)) -- Unknown Genesis KeyHash
  | DuplicateGenesisDelegateDELEG
      !(KeyHash 'GenesisDelegate (Crypto era)) -- Keyhash which is already delegated to
  | DuplicateGenesisVRFDELEG
      !(Hash (Crypto era) (VerKeyVRF (Crypto era))) --VRF KeyHash which is already delegated to
  | MirCertNotSupported (Shelley.MIRCert (Crypto era))
  deriving (Show, Eq, Generic)

instance
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  STS (DELEG era)
  where
  type State (DELEG era) = DState (Crypto era)
  type Signal (DELEG era) = Shelley.DCert (Crypto era)
  type Environment (DELEG era) = DelegEnv era
  type BaseM (DELEG era) = ShelleyBase
  type PredicateFailure (DELEG era) = DelegPredicateFailure era

  transitionRules = [delegationTransition]

instance NoThunks (DelegPredicateFailure era)

instance
  (Typeable era, Era era, Typeable (Core.Script era)) =>
  ToCBOR (DelegPredicateFailure era)
  where
  toCBOR = \case
    StakeKeyAlreadyRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR cred
    StakeKeyNotRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR cred
    StakeKeyNonZeroAccountBalanceDELEG rewardBalance ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR rewardBalance
    StakeDelegationImpossibleDELEG cred ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR cred
    WrongCertificateTypeDELEG ->
      encodeListLen 1 <> toCBOR (4 :: Word8)
    GenesisKeyNotInMappingDELEG gkh ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR gkh
    DuplicateGenesisDelegateDELEG kh ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR kh
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR vrf
    StakeKeyInRewardsDELEG cred ->
      encodeListLen 2 <> toCBOR (8 :: Word8) <> toCBOR cred
    MirCertNotSupported mirCert ->
        encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR mirCert

instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (DelegPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (DELEG era)" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, StakeKeyAlreadyRegisteredDELEG kh)
      1 -> do
        kh <- fromCBOR
        pure (2, StakeKeyNotRegisteredDELEG kh)
      2 -> do
        b <- fromCBOR
        pure (2, StakeKeyNonZeroAccountBalanceDELEG b)
      3 -> do
        kh <- fromCBOR
        pure (2, StakeDelegationImpossibleDELEG kh)
      4 -> do
        pure (1, WrongCertificateTypeDELEG)
      5 -> do
        gkh <- fromCBOR
        pure (2, GenesisKeyNotInMappingDELEG gkh)
      6 -> do
        kh <- fromCBOR
        pure (2, DuplicateGenesisDelegateDELEG kh)
      7 -> do
        vrf <- fromCBOR
        pure (2, DuplicateGenesisVRFDELEG vrf)
      8 -> do
        kh <- fromCBOR
        pure (2, StakeKeyInRewardsDELEG kh)
      9 -> do
        mirCert <- fromCBOR
        pure (2, MirCertNotSupported mirCert)
      k -> invalidKey k

-- | Same as Shelley's but without the MIR-related stuff
delegationTransition ::
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  TransitionRule (DELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr _, ds, c) <- judgmentContext
  case c of
    Shelley.DCertDeleg (RegKey hk) -> do
      eval (hk ∉ dom (_rewards ds)) ?! StakeKeyAlreadyRegisteredDELEG hk

      pure $
        ds
          { _rewards = eval (_rewards ds ∪ (singleton hk mempty)),
            _ptrs = eval (_ptrs ds ∪ (singleton ptr hk))
          }
    Shelley.DCertDeleg (DeRegKey hk) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeKeyNotRegisteredDELEG hk

      let rewardCoin = Map.lookup hk (_rewards ds)
      rewardCoin == Just mempty ?! StakeKeyNonZeroAccountBalanceDELEG rewardCoin

      pure $
        ds
          { _rewards = eval (setSingleton hk ⋪ _rewards ds),
            _delegations = eval (setSingleton hk ⋪ _delegations ds),
            _ptrs = eval (_ptrs ds ⋫ setSingleton hk)
          }
    Shelley.DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeDelegationImpossibleDELEG hk

      pure $
        ds
          { _delegations = eval (_delegations ds ⨃ (singleton hk dpool))
          }
    Shelley.DCertGenesis (GenesisDelegCert gkh vkh vrf) -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          (GenDelegs genDelegs) = _genDelegs ds

      -- gkh ∈ dom genDelegs ?! GenesisKeyNotInMappingDELEG gkh
      (case Map.lookup gkh genDelegs of Just _ -> True; Nothing -> False) ?! GenesisKeyNotInMappingDELEG gkh

      let cod =
            range $
              Map.filterWithKey (\g _ -> g /= gkh) genDelegs
          fod =
            range $
              Map.filterWithKey (\(FutureGenDeleg _ g) _ -> g /= gkh) (_fGenDelegs ds)
          currentOtherColdKeyHashes = Set.map genDelegKeyHash cod
          currentOtherVrfKeyHashes = Set.map genDelegVrfHash cod
          futureOtherColdKeyHashes = Set.map genDelegKeyHash fod
          futureOtherVrfKeyHashes = Set.map genDelegVrfHash fod

      eval (vkh ∉ (currentOtherColdKeyHashes ∪ futureOtherColdKeyHashes))
        ?! DuplicateGenesisDelegateDELEG vkh
      eval (vrf ∉ (currentOtherVrfKeyHashes ∪ futureOtherVrfKeyHashes))
        ?! DuplicateGenesisVRFDELEG vrf

      pure $
        ds
          { _fGenDelegs = eval ((_fGenDelegs ds) ⨃ (singleton (FutureGenDeleg s' gkh) (GenDelegPair vkh vrf)))
          }
    Shelley.DCertPool _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
    Shelley.DCertMir mirCert -> do
      failBecause $ MirCertNotSupported mirCert
      pure ds
