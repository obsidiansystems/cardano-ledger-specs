{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Shelley.Spec.Ledger.API.Validation
  ( ApplyBlock (..),
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
    ApplyBlock'(..)
  )
where

import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader, Reader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.Slot (SlotNo)
import Data.Proxy

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}


-- TODO: get rid of GlobalEnvironment and use BaseM directly
-- TOD: pass ApplySTSOpts
class
    ( STS (Core.EraRule "TICK" era)
    , STS (Core.EraRule "BBODY" era)
    , Environment (Core.EraRule "TICK" era) ~ ()
    , BaseM (Core.EraRule "TICK" era) ~ BaseM (Core.EraRule "BBODY" era)
    ) => ApplyBlock' era where
  type GlobalEnvironment era
  type BTError era

  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTick' :: proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "TICK" era)
    -> State (Core.EraRule "TICK" era)
  default applyTick' ::
    (BaseM (Core.EraRule "TICK" era) ~ Reader (GlobalEnvironment era))
    => proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "TICK" era)
    -> State (Core.EraRule "TICK" era)
  applyTick' _ globals state hdr =
    either err id . flip runReader globals
      . applySTS @(Core.EraRule "TICK" era)
      $ TRC ((), state, hdr)
    where
      err :: Show a => a -> b
      err msg = error $ "Panic! applyTick failed: " <> show msg
  {-# INLINE applyTick' #-}

  getBBodyState :: proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era)
  setBBodyState :: proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era) -> State (Core.EraRule "TICK" era)
  getBBodyEnv :: proxy era -> State (Core.EraRule "TICK" era) -> Environment (Core.EraRule "BBODY" era)
  wrapBlockError :: proxy era -> [[PredicateFailure (Core.EraRule "BBODY" era)]] -> BTError era

  -- | Apply the block level ledger transition.
  applyBlock' :: proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> Either (BTError era) (State (Core.EraRule "TICK" era))
  default applyBlock' ::
    (BaseM (Core.EraRule "TICK" era) ~ Reader (GlobalEnvironment era))
    => proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> Either (BTError era) (State (Core.EraRule "TICK" era))
  applyBlock' _ globals state blk =
        right (setBBodyState proxy state)
      . left (wrapBlockError proxy)
      $ res
    where
      proxy :: Proxy era
      proxy = Proxy

      res =
        flip runReader globals . applySTS @(Core.EraRule "BBODY" era) $
          TRC (getBBodyEnv proxy state, bbs, blk)
      bbs = getBBodyState proxy state
  {-# INLINE applyBlock' #-}


  -- | Re-apply a ledger block to the same state it has been applied to before.
  --
  -- This function does no validation of whether the block applies successfully;
  -- the caller implicitly guarantees that they have previously called
  -- 'applyBlockTransition' on the same block and that this was successful.
  reapplyBlock' :: proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> State (Core.EraRule "TICK" era)
  default reapplyBlock' ::
    (BaseM (Core.EraRule "TICK" era) ~ Reader (GlobalEnvironment era))
    => proxy era
    -> GlobalEnvironment era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> State (Core.EraRule "TICK" era)
  reapplyBlock' _ globals state blk =
    setBBodyState proxy state res
    where
      proxy :: Proxy era
      proxy = Proxy
      res =
        flip runReader globals . reapplySTS @(Core.EraRule "BBODY" era) $
          TRC (getBBodyEnv proxy state, bbs, blk)
      bbs = getBBodyState proxy state
  {-# INLINE reapplyBlock' #-}


instance PraosCrypto crypto => ApplyBlock' (ShelleyEra crypto) where

  type GlobalEnvironment (ShelleyEra crypto) = Globals
  type BTError (ShelleyEra crypto) = BlockTransitionError (ShelleyEra crypto)

  getBBodyState _ state = STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)
  {-# INLINE getBBodyState #-}
  setBBodyState _ = updateNewEpochState
  {-# INLINE setBBodyState #-}
  getBBodyEnv _ = mkBbodyEnv
  {-# INLINE getBBodyEnv #-}
  wrapBlockError _ = BlockTransitionError . join
  {-# INLINE wrapBlockError #-}



class
  ( BaseM (Core.EraRule "TICK" era) ~ BaseM (Core.EraRule "BBODY" era),
    Signal (Core.EraRule "BBODY" era) ~ Block era,
    BTError era ~ BlockTransitionError era,
    ChainData (Block era),
    AnnotatedData (Block era),
    ChainData (BHeader (Crypto era)),
    AnnotatedData (BHeader (Crypto era)),
    ChainData (NewEpochState era),
    SerialisableData (NewEpochState era),
    ChainData (BlockTransitionError era),
    ChainData (STS.PredicateFailure (STS.CHAIN era)),
    STS (Core.EraRule "TICK" era),
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    STS (Core.EraRule "BBODY" era)
  ) =>
  ApplyBlock era
  where
  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTick ::
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  default applyTick :: (ApplyBlock' era, GlobalEnvironment era ~ Globals) =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  applyTick = applyTick' (Proxy :: Proxy era)
  {-# INLINE applyTick #-}

  -- | Apply the block level ledger transition.
  applyBlock ::
    MonadError (BlockTransitionError era) m =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  default applyBlock ::  (ApplyBlock' era, GlobalEnvironment era ~ Globals) =>
    (MonadError (BlockTransitionError era) m) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  applyBlock globals state blk =
    liftEither
      $ applyBlock' (Proxy :: Proxy era) globals state blk
  {-# INLINE applyBlock #-}

  -- | Re-apply a ledger block to the same state it has been applied to before.
  --
  -- This function does no validation of whether the block applies successfully;
  -- the caller implicitly guarantees that they have previously called
  -- 'applyBlockTransition' on the same block and that this was successful.
  reapplyBlock ::
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  default reapplyBlock :: (ApplyBlock' era, GlobalEnvironment era ~ Globals) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  reapplyBlock = reapplyBlock' (Proxy :: Proxy era)
  {-# INLINE reapplyBlock #-}

instance PraosCrypto crypto => ApplyBlock (ShelleyEra crypto)

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}

chainChecks ::
  forall era m.
  ( Era era,
    MonadError (STS.PredicateFailure (STS.CHAIN era)) m
  ) =>
  Globals ->
  STS.ChainChecksData ->
  BHeader (Crypto era) ->
  m ()
chainChecks globals ccd bh = STS.chainChecks (maxMajorPV globals) ccd bh

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkBbodyEnv ::
  NewEpochState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodyPp = LedgerState.esPp nesEs,
        STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

updateNewEpochState ::
  NewEpochState era ->
  STS.BbodyState era ->
  NewEpochState era
updateNewEpochState ss (STS.BbodyState ls bcur) =
  LedgerState.updateNES ss bcur ls

newtype TickTransitionError era
  = TickTransitionError [STS.PredicateFailure (Core.EraRule "TICK" era)]
  deriving (Generic)

instance
  (NoThunks (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  NoThunks (TickTransitionError era)

deriving stock instance
  (Eq (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  Eq (TickTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  Show (TickTransitionError era)

newtype BlockTransitionError era
  = BlockTransitionError [STS.PredicateFailure (Core.EraRule "BBODY" era)]
  deriving (Generic)

deriving stock instance
  (Eq (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  Eq (BlockTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  Show (BlockTransitionError era)

instance
  (NoThunks (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  NoThunks (BlockTransitionError era)
