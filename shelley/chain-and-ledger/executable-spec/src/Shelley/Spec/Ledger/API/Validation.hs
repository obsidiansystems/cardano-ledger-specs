{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE TupleSections #-}

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Shelley.Spec.Ledger.API.Validation
  ( ApplyBlock (..),
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
    ApplyBlock'(..),
    runApplyBlockData, ApplyBlockData(..),
    TraceApplyBlock(..),
    ModelTxId(..), ModelAddress(..), ModelValue(..), ModelTxIn(..),
    ModelTxOut(..), ModelTx(..), ModelBlock(..)
  )
where

import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.Slot (SlotNo)
import Data.Proxy
import Numeric.Natural
import Data.Set (Set)
import Data.Bifunctor
-- import Data.Default.Class
-- import qualified Data.Map as Map
-- import qualified Cardano.Ledger.Crypto as CC
-- import qualified Cardano.Crypto.Hash.Class as CC
-- import Control.Monad.State (evalStateT)
-- import Data.Functor.Identity
-- import Data.Kind (Type)
-- import Control.Monad.State (StateT)
-- import Data.Map (Map)

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}


newtype ModelTxId = ModelTxId Integer
  deriving (Eq, Ord, Show)
newtype ModelAddress = ModelAddress String
  deriving (Eq, Ord, Show)
newtype ModelValue = ModelValue { unModelValue :: Integer }
  deriving (Eq, Ord, Show)

data ModelTxIn = ModelTxIn ModelTxId Natural
data ModelTxOut = ModelTxOut ModelAddress ModelValue

data ModelTx = ModelTx
  { _mtxId :: !ModelTxId
  , _mtxInputs :: !(Set ModelTxIn)
  , _mtxOutputs :: ![ModelTxOut]
  , _mtxFee :: !ModelValue
  }

data ModelBlock = ModelBlock
  { _mbSlot :: !SlotNo
  , _mbUtxo :: ![ModelTx]
  }


class TraceApplyBlock era where
  toEra
    :: proxy era
    -> [ModelTxOut]
    -> [ModelBlock]
    -> BaseM (Core.EraRule "TICK" era)
      ( State (Core.EraRule "TICK" era)
      , [ApplyBlockData era]
      )


data ApplyBlockData era where
  ApplyTick :: Signal (Core.EraRule "TICK" era) -> ApplyBlockData era
  ApplyBlock :: Signal (Core.EraRule "BBODY" era) -> ApplyBlockData era


data ApplyBlockTransitionError era
   = ApplyBlockTransitionError_Block (BlockTransitionError era)
   | ApplyBlockTransitionError_Tick (TickTransitionError era)

type ApplyBlockError era =
  ( ApplyBlockTransitionError era
  , State (Core.EraRule "TICK" era)
  , ApplyBlockData era
  )

runApplyBlockData
  :: forall era. ApplyBlock' era
  => ApplySTSOpts
  -> State (Core.EraRule "TICK" era)
  -> [ApplyBlockData era]
  -> BaseM (Core.EraRule "TICK" era)
    (Either (ApplyBlockError era) (State (Core.EraRule "TICK" era)))
runApplyBlockData opts = go
  where
    loop
      :: ([a] -> (ApplyBlockTransitionError era))
      -> ApplyBlockData era
      -> [ApplyBlockData era]
      -> (State (Core.EraRule "TICK" era), [[a]])
      -> BaseM (Core.EraRule "TICK" era)
        (Either (ApplyBlockError era) (State (Core.EraRule "TICK" era)))
    loop _ _ xs (good, []) = go good xs
    loop c x _ (st, bad) = pure . Left . (,st,x) . c $ join bad
    {-# INLINE loop #-}

    go
      :: State (Core.EraRule "TICK" era)
      -> [ApplyBlockData era]
      -> BaseM (Core.EraRule "TICK" era)
        (Either (ApplyBlockError era) (State (Core.EraRule "TICK" era)))
    go st [] = pure (Right st)
    go st0 (x:xs) = do
      case x of
        ApplyTick tick -> applyTickOpts' opts (Proxy :: Proxy era) st0 tick
          >>= loop (ApplyBlockTransitionError_Tick . TickTransitionError) x xs
        ApplyBlock blk -> applyBlockOpts' opts (Proxy :: Proxy era) st0 blk
          >>= loop (ApplyBlockTransitionError_Block . BlockTransitionError) x xs

-- TODO: move implementations out of class
-- TODO: move defaulting methods to a suitable "deriving via" newtype
class
    ( STS (Core.EraRule "TICK" era)
    , STS (Core.EraRule "BBODY" era)
    , Environment (Core.EraRule "TICK" era) ~ ()
    , BaseM (Core.EraRule "TICK" era) ~ BaseM (Core.EraRule "BBODY" era)
    ) => ApplyBlock' era where

  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTick' :: proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "TICK" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era))
  applyTick' proxy st0 sig = flip fmap (applyTickOpts' defaultOpts proxy st0 sig) $ \case
    (st, []) -> st
    (_, pfs) -> error $ "Panic! applyTick failed: " <> show pfs
    where
      defaultOpts = ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateAll
        }
  {-# INLINE applyTick' #-}

  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTickOpts' :: ApplySTSOpts
    -> proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "TICK" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era), [[PredicateFailure (Core.EraRule "TICK" era)]])
  default applyTickOpts' :: ApplySTSOpts
    -> proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "TICK" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era), [[PredicateFailure (Core.EraRule "TICK" era)]])
  applyTickOpts' opts _ state hdr = applySTSOpts @(Core.EraRule "TICK" era) opts
      ( TRC ((), state, hdr) )
  {-# INLINE applyTickOpts' #-}


  getBBodyState :: proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era)
  default getBBodyState ::
    ( State (Core.EraRule "BBODY" era) ~ STS.BbodyState era
    , State (Core.EraRule "TICK" era) ~ NewEpochState era
    ) => proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era)
  getBBodyState _ state = STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)
  {-# INLINE getBBodyState #-}

  setBBodyState :: proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era) -> State (Core.EraRule "TICK" era)
  default setBBodyState ::
    ( State (Core.EraRule "BBODY" era) ~ STS.BbodyState era
    , State (Core.EraRule "TICK" era) ~ NewEpochState era
    ) => proxy era -> State (Core.EraRule "TICK" era) -> State (Core.EraRule "BBODY" era) -> State (Core.EraRule "TICK" era)
  setBBodyState _ = updateNewEpochState
  {-# INLINE setBBodyState #-}
  getBBodyEnv :: proxy era -> State (Core.EraRule "TICK" era) -> Environment (Core.EraRule "BBODY" era)
  default getBBodyEnv ::
    ( State (Core.EraRule "TICK" era) ~ NewEpochState era
    , Environment (Core.EraRule "BBODY" era) ~ STS.BbodyEnv era
    ) => proxy era -> State (Core.EraRule "TICK" era) -> Environment (Core.EraRule "BBODY" era)
  getBBodyEnv _ = mkBbodyEnv
  {-# INLINE getBBodyEnv #-}

  -- | Apply the block level ledger transition.
  applyBlock' :: proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> BaseM (Core.EraRule "TICK" era) (Either (BlockTransitionError era) (State (Core.EraRule "TICK" era)))
  applyBlock' proxy st0 sig = flip fmap (applyBlockOpts' defaultOpts proxy st0 sig) $ \case
    (st, []) -> Right st
    (_, pfs) -> Left . BlockTransitionError $ join pfs
    where
      defaultOpts = ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateAll
        }
  {-# INLINE applyBlock' #-}

  -- | Apply the block level ledger transition.
  applyBlockOpts' :: ApplySTSOpts -> proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era), [[PredicateFailure (Core.EraRule "BBODY" era)]])
  applyBlockOpts' opts _ state blk = first (setBBodyState proxy state) <$> res
    where
      proxy :: Proxy era
      proxy = Proxy

      res = applySTSOpts @(Core.EraRule "BBODY" era) opts $
          TRC (getBBodyEnv proxy state, bbs, blk)
      bbs = getBBodyState proxy state
  {-# INLINE applyBlockOpts' #-}


  -- | Re-apply a ledger block to the same state it has been applied to before.
  --
  -- This function does no validation of whether the block applies successfully;
  -- the caller implicitly guarantees that they have previously called
  -- 'applyBlockTransition' on the same block and that this was successful.
  reapplyBlock' :: proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era))
  default reapplyBlock' :: proxy era
    -> State (Core.EraRule "TICK" era)
    -> Signal (Core.EraRule "BBODY" era)
    -> BaseM (Core.EraRule "TICK" era) (State (Core.EraRule "TICK" era))
  reapplyBlock' _ state blk =
    setBBodyState proxy state <$> res
    where
      proxy :: Proxy era
      proxy = Proxy
      res = reapplySTS @(Core.EraRule "BBODY" era) $
          TRC (getBBodyEnv proxy state, bbs, blk)
      bbs = getBBodyState proxy state
  {-# INLINE reapplyBlock' #-}




instance PraosCrypto crypto => ApplyBlock' (ShelleyEra crypto) where

  -- getBBodyState _ state = STS.BbodyState
  --         (LedgerState.esLState $ LedgerState.nesEs state)
  --         (LedgerState.nesBcur state)
  -- {-# INLINE getBBodyState #-}
  -- setBBodyState _ = updateNewEpochState
  -- {-# INLINE setBBodyState #-}
  -- getBBodyEnv _ = mkBbodyEnv
  -- {-# INLINE getBBodyEnv #-}

class
  ( Signal (Core.EraRule "BBODY" era) ~ Block era,
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
    STS (Core.EraRule "BBODY" era),
    BaseM (Core.EraRule "TICK" era) ~ ShelleyBase,
    BaseM (Core.EraRule "BBODY" era) ~ ShelleyBase
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
  default applyTick :: ApplyBlock' era =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  applyTick globals state slot = runReader (applyTick' (Proxy :: Proxy era) state slot) globals
  {-# INLINE applyTick #-}

  -- | Apply the block level ledger transition.
  applyBlock ::
    MonadError (BlockTransitionError era) m =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  default applyBlock :: ApplyBlock' era =>
    (MonadError (BlockTransitionError era) m) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  applyBlock globals state blk =
    liftEither
      $ runReader (applyBlock' (Proxy :: Proxy era) state blk) globals
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
  default reapplyBlock :: ApplyBlock' era =>
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  reapplyBlock globals state blk = runReader (reapplyBlock' (Proxy :: Proxy era) state blk) globals
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
