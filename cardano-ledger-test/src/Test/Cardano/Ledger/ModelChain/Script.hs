{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.ModelChain.Script where

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Keys
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Slotting.Slot hiding (at)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Void
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.ModelChain.Address

data TyScriptFeature = TyScriptFeature
  { _tyScript_timelock :: !Bool,
    _tyScript_plutus :: !Bool
  }

data ModelScript (k :: TyScriptFeature) where
  ModelScript_Timelock :: ModelTimelock -> ModelScript ('TyScriptFeature 'True x)
  ModelScript_PlutusV1 :: ModelPlutusScript -> ModelScript ('TyScriptFeature x 'True)

deriving instance Eq (ModelScript k)

deriving instance Ord (ModelScript k)

deriving instance Show (ModelScript k)

data ModelPlutusScript
  = ModelPlutusScript_AlwaysSucceeds Natural
  | ModelPlutusScript_AlwaysFails Natural
  deriving (Eq, Ord, Show)

modelScriptNeededSigs :: ModelTimelock -> [ModelAddress]
modelScriptNeededSigs = go
  where
    go = \case
      ModelTimelock_Signature ma -> [ma]
      ModelTimelock_AllOf xs -> go =<< xs
      ModelTimelock_AnyOf xs -> go =<< take 1 xs
      ModelTimelock_MOfN n xs -> go =<< take n xs
      ModelTimelock_TimeStart _ -> []
      ModelTimelock_TimeExpire _ -> []

-- modelScriptNeededSigs (ModelScript_PlutusV1 {}) = []

data ScriptFeatureTag (s :: TyScriptFeature) where
  ScriptFeatureTag_None :: ScriptFeatureTag ('TyScriptFeature 'False 'False)
  ScriptFeatureTag_Simple :: ScriptFeatureTag ('TyScriptFeature 'True 'False)
  ScriptFeatureTag_PlutusV1 :: ScriptFeatureTag ('TyScriptFeature 'True 'True)

class KnownScriptFeature (s :: TyScriptFeature) where reifyScriptFeature :: proxy s -> ScriptFeatureTag s

instance KnownScriptFeature ('TyScriptFeature 'False 'False) where reifyScriptFeature _ = ScriptFeatureTag_None

instance KnownScriptFeature ('TyScriptFeature 'True 'False) where reifyScriptFeature _ = ScriptFeatureTag_Simple

instance KnownScriptFeature ('TyScriptFeature 'True 'True) where reifyScriptFeature _ = ScriptFeatureTag_PlutusV1

hasKnownScriptFeature :: ScriptFeatureTag s -> (KnownScriptFeature s => c) -> c
hasKnownScriptFeature = \case
  ScriptFeatureTag_None -> \x -> x
  ScriptFeatureTag_Simple -> \x -> x
  ScriptFeatureTag_PlutusV1 -> \x -> x

-- TODO: get rid of these type families, use GADTs everywhere
type family IfSupportsTimelock a (k :: TyScriptFeature) where
  IfSupportsTimelock a ('TyScriptFeature 'True _) = a
  IfSupportsTimelock _ ('TyScriptFeature 'False _) = Void

type family IfSupportsPlutus a (k :: TyScriptFeature) where
  IfSupportsPlutus a ('TyScriptFeature _ 'True) = a
  IfSupportsPlutus _ ('TyScriptFeature _ 'False) = Void

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'False b = b
  Or 'True _ = 'True

data IfSupportsPlutus' a b (k :: TyScriptFeature) where
  NoPlutusSupport :: a -> IfSupportsPlutus' a b ('TyScriptFeature x 'False)
  SupportsPlutus :: b -> IfSupportsPlutus' a b ('TyScriptFeature x 'True)

ifSupportsPlutus ::
  KnownScriptFeature s =>
  proxy s ->
  a ->
  b ->
  IfSupportsPlutus' a b s
ifSupportsPlutus proxy x y = case reifySupportsPlutus proxy of
  NoPlutusSupport () -> NoPlutusSupport x
  SupportsPlutus () -> SupportsPlutus y

mapSupportsPlutus ::
  (a -> b) ->
  IfSupportsPlutus' x a s ->
  IfSupportsPlutus' x b s
mapSupportsPlutus f = \case
  NoPlutusSupport x -> NoPlutusSupport x
  SupportsPlutus x -> SupportsPlutus (f x)

reifySupportsPlutus ::
  KnownScriptFeature s => proxy s -> IfSupportsPlutus' () () s
reifySupportsPlutus proxy = case reifyScriptFeature proxy of
  ScriptFeatureTag_None -> NoPlutusSupport ()
  ScriptFeatureTag_Simple -> NoPlutusSupport ()
  ScriptFeatureTag_PlutusV1 -> SupportsPlutus ()

data IfSupportsScript a b (k :: TyScriptFeature) where
  NoScriptSupport ::
    a ->
    IfSupportsScript a b ('TyScriptFeature 'False 'False)
  SupportsScript ::
    (Or t p ~ 'True) =>
    ScriptFeatureTag ('TyScriptFeature t p) ->
    b ->
    IfSupportsScript a b ('TyScriptFeature t p)

-- TODO: start/expire are somewhat irritating since absolute slot numbers aren't
-- visible in the model; it should probably be refactored to use epochs + slot
-- in epoch
data ModelTimelock
  = ModelTimelock_Signature ModelAddress
  | ModelTimelock_AllOf [ModelTimelock]
  | ModelTimelock_AnyOf [ModelTimelock]
  | ModelTimelock_MOfN Int [ModelTimelock] -- Note that the Int may be negative in which case (MOfN -2 [..]) is always True
  | ModelTimelock_TimeStart SlotNo -- The start time
  | ModelTimelock_TimeExpire SlotNo -- The time it expires
  deriving (Eq, Ord, Show)

elaborateModelTimelock ::
  forall crypto m.
  (C.Crypto crypto, Applicative m) =>
  (ModelAddress -> m (KeyHash 'Witness crypto)) ->
  ModelTimelock ->
  m (Timelock crypto)
elaborateModelTimelock f = go
  where
    go :: ModelTimelock -> m (Timelock crypto)
    go = \case
      ModelTimelock_Signature maddr -> RequireSignature <$> f maddr
      ModelTimelock_AllOf xs -> RequireAllOf . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_AnyOf xs -> RequireAnyOf . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_MOfN m xs -> RequireMOf m . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_TimeStart slotNo -> pure $ RequireTimeStart slotNo
      ModelTimelock_TimeExpire slotNo -> pure $ RequireTimeExpire slotNo

elaborateModelScript ::
  ModelPlutusScript ->
  Alonzo.Script era
elaborateModelScript = \case
  ModelPlutusScript_AlwaysSucceeds n -> Alonzo.alwaysSucceeds n
  ModelPlutusScript_AlwaysFails n -> Alonzo.alwaysFails n
