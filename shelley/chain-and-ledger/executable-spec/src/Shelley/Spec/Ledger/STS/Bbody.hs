{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Bbody
  ( BBODY,
    BbodyState (..),
    BbodyEnv (..),
    PredicateFailure (..),
    State,
  )
where

import Data.Proxy (Proxy (..))
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    trans,
    (?!),
  )
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    Block (..),
    HashBBody,
    TxSeq (..),
    bBodySize,
    bbHash,
    hBbsize,
    incrBlocks,
    poolIDfromBHBody,
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    LedgerState,
  )
import Shelley.Spec.Ledger.OverlaySchedule
  ( OverlaySchedule,
    isOverlaySlot,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv (..))
import Shelley.Spec.Ledger.Tx (TxBody)

data BBODY crypto

data BbodyState crypto
  = BbodyState (LedgerState crypto) (BlocksMade crypto)
  deriving (Eq, Show)

data BbodyEnv crypto = BbodyEnv
  { bbodySlots :: OverlaySchedule crypto,
    bbodyPp :: PParams,
    bbodyAccount :: AccountState
  }

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  STS (BBODY crypto)
  where
  type
    State (BBODY crypto) =
      BbodyState crypto

  type
    Signal (BBODY crypto) =
      Block crypto

  type Environment (BBODY crypto) = BbodyEnv crypto

  type BaseM (BBODY crypto) = ShelleyBase

  data PredicateFailure (BBODY crypto)
    = WrongBlockBodySizeBBODY
        !Int -- Actual Body Size
        !Int -- Claimed Body Size in Header
    | InvalidBodyHashBBODY
        !(HashBBody crypto) -- Actual Hash
        !(HashBBody crypto) -- Claimed Hash
    | Stuff !String
    | LedgersFailure (PredicateFailure (LEDGERS crypto)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [bbodyTransition]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (BBODY crypto))

bbodyTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (BBODY crypto)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv oslots pp account,
               BbodyState ls b,
               Block (BHeader bhb _) txsSeq
               )
           ) -> do
        failBecause $
          Stuff ( "VRF vkey size: "
                    <> (show $ VRF.sizeVerKeyVRF (Proxy @(VRF crypto)))
                    <> "\nVRF proof size: "
                    <> (show $ VRF.sizeCertVRF (Proxy @(VRF crypto)))
                    <> "\nKES vkey size: "
                    <> (show $ KES.sizeVerKeyKES (Proxy @(KES crypto)))
                    <> "\nKES sig size: "
                    <> (show $ KES.sizeSigKES (Proxy @(KES crypto)))
                )
        let TxSeq txs = txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = bbHash txsSeq

        actualBodySize == fromIntegral (hBbsize bhb)
          ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ hBbsize bhb)

        actualBodyHash == bhash bhb ?! InvalidBodyHashBBODY actualBodyHash (bhash bhb)

        ls' <-
          trans @(LEDGERS crypto) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp account, ls, StrictSeq.getSeq txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . poolIDfromBHBody $ bhb
        pure $ BbodyState ls' (incrBlocks (isOverlaySlot (bheaderSlotNo bhb) oslots) hkAsStakePool b)

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (LEDGERS crypto) (BBODY crypto)
  where
  wrapFailed = LedgersFailure
