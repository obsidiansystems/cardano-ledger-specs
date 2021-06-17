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
    DCert (..),
    DelegEnv (..),
    PredicateFailure,
    DelegPredicateFailure (..),
    fromShelleyCert,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
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
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
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
    Ptr, PoolCert
  )
import Cardano.Prelude (NFData)

-- | Same as Shelley's except no 'DCertMir'
data DCert crypto
  = DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (DCert crypto)

fromShelleyCert :: Shelley.DCert crypto -> DCert crypto
fromShelleyCert (Shelley.DCertDeleg dc) = DCertDeleg dc
fromShelleyCert (Shelley.DCertPool pc) = DCertPool pc
fromShelleyCert (Shelley.DCertGenesis gdc) = DCertGenesis gdc
fromShelleyCert (Shelley.DCertMir dc) = error $ "BUG: Prototype two does not support MIR-certificates. " <> show dc

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
  deriving (Show, Eq, Generic)

instance
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  STS (DELEG era)
  where
  type State (DELEG era) = DState (Crypto era)
  type Signal (DELEG era) = DCert (Crypto era)
  type Environment (DELEG era) = DelegEnv era
  type BaseM (DELEG era) = ShelleyBase
  type PredicateFailure (DELEG era) = DelegPredicateFailure era

  transitionRules = [delegationTransition]

instance NoThunks (DelegPredicateFailure era)

-- | TODO
instance
  (Typeable era, Era era, Typeable (Core.Script era)) =>
  ToCBOR (DelegPredicateFailure era)
  where
  toCBOR = error "TODO"

-- | TODO
instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (DelegPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (DELEG era)" $
    error "TODO"

-- | Same as Shelley's but without the MIR-related stuff
delegationTransition ::
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  TransitionRule (DELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr _, ds, c) <- judgmentContext
  case c of
    DCertDeleg (RegKey hk) -> do
      eval (hk ∉ dom (_rewards ds)) ?! StakeKeyAlreadyRegisteredDELEG hk

      pure $
        ds
          { _rewards = eval (_rewards ds ∪ (singleton hk mempty)),
            _ptrs = eval (_ptrs ds ∪ (singleton ptr hk))
          }
    DCertDeleg (DeRegKey hk) -> do
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
    DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeDelegationImpossibleDELEG hk

      pure $
        ds
          { _delegations = eval (_delegations ds ⨃ (singleton hk dpool))
          }
    DCertGenesis (GenesisDelegCert gkh vkh vrf) -> do
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
    DCertPool _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
