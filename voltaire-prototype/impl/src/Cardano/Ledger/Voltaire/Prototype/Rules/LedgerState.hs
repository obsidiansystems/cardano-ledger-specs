{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : LedgerState
-- Description : Almost entirely copy/paste from 'Shelley.Spec.Ledger.LedgerState'
module Cardano.Ledger.Voltaire.Prototype.Rules.LedgerState
( witsVKeyNeeded
)
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.Address (Addr (..), bootstrapKeyHash)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe (..),
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    delegCWitness,
    genesisCWitness,
    poolCWitness,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
    asWitness
  )
import Shelley.Spec.Ledger.Tx
  ( extractKeyHashWitnessSet,
  )
import Shelley.Spec.Ledger.TxBody
  ( PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    TxIn (..),
    Wdrl (..),
    getRwdCred,
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    txinLookup,
  )
import Shelley.Spec.Ledger.LedgerState (WitHashes(WitHashes))
import Cardano.Ledger.Voltaire.Prototype.Class (VoltaireClass (proposalKeyHash), Update)

-- | NB: Only difference between this and 
-- 'Shelley.Spec.Ledger.LedgerState.witsVKeyNeeded'
-- is how @updateKeys@ is calculated (via 'proposalKeyHash' of 'VoltaireClass').
witsVKeyNeeded ::
  forall era tx.
  ( Era era,
    VoltaireClass era,
    HasField "body" tx (Core.TxBody era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  UTxO era ->
  tx ->
  GenDelegs (Crypto era) ->
  WitHashes (Crypto era)
witsVKeyNeeded utxo' tx genDelegs =
  WitHashes $
    certAuthors
      `Set.union` inputAuthors
      `Set.union` owners
      `Set.union` wdrlAuthors
      `Set.union` updateKeys
  where
    txbody = getField @"body" tx
    inputAuthors :: Set (KeyHash 'Witness (Crypto era))
    inputAuthors = foldr accum Set.empty (getField @"inputs" txbody)
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just out ->
              case getField @"address" out of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (getField @"wdrls" txbody))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr accum Set.empty (getField @"certs" txbody)
      where
        accum (DCertPool (RegPool pool)) ans =
          Set.union
            (Set.map asWitness (_poolOwners pool))
            ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (Crypto era))
    certAuthors = foldr accum Set.empty (getField @"certs" txbody)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (Crypto era))
    updateKeys =
        maybe Set.empty proposalKeyHash (strictMaybeToMaybe $ getField @"update" txbody)
