{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Cardano.Ledger.Voltaire.Prototype.Rules.Two.Delpl
-- Description : The same as 'Shelley.Spec.Ledger.STS.Delpl'
--               except different environment and state type
--               for DELEG.
module Cardano.Ledger.Voltaire.Prototype.Rules.Two.Delpl
  ( DELPL,
    Shelley.DelplEnv (..),
    Shelley.DelplPredicateFailure (..),
    PredicateFailure,
  )
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Control.State.Transition
import GHC.Records (HasField)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.LedgerState
  ( DPState,
    DState,
    PState,
    _dstate,
    _pstate,
  )
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.STS.Delpl as Shelley
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Deleg as Two
import Shelley.Spec.Ledger.STS.Pool (POOL, PoolEnv (..), PoolPredicateFailure)
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    GenesisDelegCert (..),
    PoolCert (..),
  )

data DELPL era

instance
  ( Era era,
    Embed (Core.EraRule "DELEG" era) (DELPL era),
    Environment (Core.EraRule "DELEG" era) ~ (Two.DelegEnv era),
    State (Core.EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (Core.EraRule "DELEG" era) ~ Two.DCert (Crypto era),
    Embed (Core.EraRule "POOL" era) (DELPL era),
    Environment (Core.EraRule "POOL" era) ~ PoolEnv era,
    State (Core.EraRule "POOL" era) ~ PState (Crypto era),
    Signal (Core.EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  STS (DELPL era)
  where
  type State (DELPL era) = DPState (Crypto era)
  type Signal (DELPL era) = DCert (Crypto era)
  type Environment (DELPL era) = Shelley.DelplEnv era
  type BaseM (DELPL era) = ShelleyBase
  type PredicateFailure (DELPL era) = Shelley.DelplPredicateFailure era

  transitionRules = [delplTransition]

delplTransition ::
  forall era.
  ( Embed (Core.EraRule "DELEG" era) (DELPL era),
    Environment (Core.EraRule "DELEG" era) ~ (Two.DelegEnv era),
    State (Core.EraRule "DELEG" era) ~ DState (Crypto era),
    Signal (Core.EraRule "DELEG" era) ~ Two.DCert (Crypto era),
    Embed (Core.EraRule "POOL" era) (DELPL era),
    Environment (Core.EraRule "POOL" era) ~ PoolEnv era,
    State (Core.EraRule "POOL" era) ~ PState (Crypto era),
    Signal (Core.EraRule "POOL" era) ~ DCert (Crypto era)
  ) =>
  TransitionRule (DELPL era)
delplTransition = do
  TRC (Shelley.DelplEnv slot ptr pp _, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(Core.EraRule "POOL" era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(Core.EraRule "POOL" era) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d {_pstate = ps}
    DCertGenesis (GenesisDelegCert {}) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (Two.DelegEnv slot ptr pp, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (Two.DelegEnv slot ptr pp, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (Two.DelegEnv slot ptr pp, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(Core.EraRule "DELEG" era) $ TRC (Two.DelegEnv slot ptr pp, _dstate d, c)
      pure $ d {_dstate = ds}
    DCertMir _ -> do
      ds <- trans @(Core.EraRule "DELEG" era) $ TRC (Two.DelegEnv slot ptr pp, _dstate d, c)
      pure $ d {_dstate = ds}

instance
  ( Era era,
    STS (POOL era),
    PredicateFailure (Core.EraRule "POOL" era) ~ PoolPredicateFailure era
  ) =>
  Embed (POOL era) (DELPL era)
  where
  wrapFailed = Shelley.PoolFailure

instance
  ( Era era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    PredicateFailure (Core.EraRule "DELEG" era) ~ Two.DelegPredicateFailure era
  ) =>
  Embed (Two.DELEG era) (DELPL era)
  where
  wrapFailed = Shelley.DelegFailure
