{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.ApplyBlock where

import Control.Monad.State
import Data.Functor.Identity
import Data.Proxy
import Data.Traversable
import Numeric.Natural
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence.Strict as StrictSeq

import Control.State.Transition.Extended
import Cardano.Ledger.Shelley (ShelleyEra)
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..)
  , StrictMaybe (..)
  )
import Shelley.Spec.Ledger.BaseTypes (Nonce (..))
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.TxBody
import Shelley.Spec.Ledger.LedgerState
import qualified Cardano.Crypto.Hash.Class as CHC
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (KES, DSIGN)
import qualified Shelley.Spec.Ledger.UTxO as UTxO
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Tx
-- import Cardano.Ledger.Era
import Cardano.Slotting.Block

import Test.Shelley.Spec.Ledger.Generator.ScriptClass
import Test.Shelley.Spec.Ledger.Generator.Core
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Generator.Presets
import Shelley.Spec.Ledger.OCert

type KeyPair' crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

data ShelleyEraState crypto = ShelleyEraState
  { _unusedKeyPairs :: [KeyPair' crypto]
  , _keys :: Map.Map ModelAddress (KeyPair' crypto)
  , _txIds :: Map.Map ModelTxId (TxId crypto)
  , _blockNo :: BlockNo
  , _prevHash :: HashHeader crypto
  }

getKeyPairFor :: forall m crypto. MonadState (ShelleyEraState crypto) m => ModelAddress -> m (KeyPair' crypto)
getKeyPairFor mAddr = do
  st <- get
  case Map.lookup mAddr (_keys st) of
    Just k -> pure k
    Nothing -> case _unusedKeyPairs st of
      [] -> error "ran out of keys"
      (k:ks) -> do
        put $ st {_unusedKeyPairs = ks, _keys = Map.insert mAddr k $ _keys st}
        pure k

instance
    ( PraosCrypto crypto
    , KES.Signable (KES crypto) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
    ) => TraceApplyBlock (ShelleyEra crypto) where
  toEra _ utxos blocks = do
    nes <- reapplySTS @(Core.EraRule "TICK" (ShelleyEra crypto)) (IRC ())
    let
      initialState :: ShelleyEraState crypto
      initialState = ShelleyEraState
        { _unusedKeyPairs = mkKeyPairs <$> [1..]
        , _keys = Map.empty
        , _txIds = Map.singleton (ModelTxId 0) genesisHash
        , _blockNo = 0
        , _prevHash = genesisHash'
        }

      myGenEnv = genEnv (Proxy :: Proxy (ShelleyEra crypto))

      set :: ((a -> Identity b) -> (s -> Identity t)) -> b -> s -> t
      set l b s = runIdentity (l (\_ -> Identity b) s)

      genesisHash = TxId $ CHC.castHash $ CHC.hashWith id "TEST GENESIS"
      genesisHash' = HashHeader $ CHC.castHash $ CHC.hashWith id "TEST GENESIS"

      getBHeaderHash :: Block (ShelleyEra crypto) -> HashHeader crypto
      getBHeaderHash (Block bh _) = bhHash bh

      mkGenTxOut
        :: forall m. (MonadState (ShelleyEraState crypto) m)
        => (ModelTxOut, Natural)
        -> m (TxIn crypto, TxOut (ShelleyEra crypto))
      mkGenTxOut (mtxOut, n) =
        fmap ((,) (TxIn genesisHash n)) $ mkTxOut mtxOut

      mkTxOut
        :: forall m. (MonadState (ShelleyEraState crypto) m)
        => ModelTxOut
        -> m (TxOut (ShelleyEra crypto))
      mkTxOut (ModelTxOut mAddr (ModelValue mValue)) = do
        addr <- getKeyPairFor mAddr
        pure (TxOut (toAddr Testnet addr) (Coin mValue))

      mkTx
        :: forall m. (MonadState (ShelleyEraState crypto) m)
        => ModelTx
        -> m (Tx (ShelleyEra crypto))
      mkTx mtx@(ModelTx {}) = do
        ses <- get
        outs <- traverse mkTxOut $ _mtxOutputs mtx
        let
          realTxBody = TxBody
            { _inputs = foldMap (\(ModelTxIn mtxId idx) -> maybe Set.empty Set.singleton $ TxIn <$>
                                    (Map.lookup mtxId $ _txIds ses) <*> pure idx) $ _mtxInputs mtx
            , _outputs = StrictSeq.fromList outs
            , _certs = StrictSeq.empty
            , _wdrls = Wdrl Map.empty
            , _txfee = Coin . unModelValue $ _mtxFee mtx
            , _ttl = 0
            , _txUpdate = SNothing
            , _mdHash = SNothing
            }
        put ses { _txIds = Map.insert (_mtxId mtx) (UTxO.txid realTxBody) (_txIds ses)}
        pure (Tx realTxBody mempty SNothing)

      mkBlock'
        :: forall m. (MonadState (ShelleyEraState crypto) m)
        => ModelBlock -> m (Block (ShelleyEra crypto))
      mkBlock' mBlk = do
        txSeq :: [Tx (ShelleyEra era)] <- for (_mbUtxo mBlk) mkTx

        st <- get
        -- let
        let
          newBlock = mkBlock
            (_prevHash st)
            (head . ksStakePools . geKeySpace $ myGenEnv)
            txSeq
            (_mbSlot mBlk)
            (_blockNo st)
            NeutralNonce
            0
            0
            (mkOCert (head . ksGenesisDelegates . geKeySpace $ myGenEnv) 0 (KESPeriod 0))
        put $ st {_prevHash = getBHeaderHash newBlock }
        pure newBlock


    flip evalStateT initialState $ do
      genesisUtxos <- traverse mkGenTxOut (zip utxos [0..])
      let
        nes' = set (nesEsLens . esLStateLens) ledgerState nes
        ledgerState = genesisState @(ShelleyEra crypto) Map.empty
          . UTxO.UTxO
          $ Map.fromList genesisUtxos

      blocks' :: [[ApplyBlockData (ShelleyEra crypto)]] <- for blocks $ \block -> do
        newBlock <- mkBlock' block
        pure $
          [ ApplyTick $ _mbSlot block
          , ApplyBlock newBlock
          ]

      pure $ (nes', concat blocks')
