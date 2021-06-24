{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{- We disable warnings for name shadowing because of
https://gitlab.haskell.org/ghc/ghc/-/issues/14630, which means that we get
shadowing warnings for the named field puns when used with a pattern synonym.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Voltaire.Prototype.One.Translation
()
where

import Cardano.Binary
  ( DecoderError,
    decodeAnnotator,
    fromCBOR,
    serialize,
  )
import qualified Cardano.Ledger.Voltaire.Prototype as One
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AuxiliaryData (..),
    pattern AuxiliaryData,
  )
import Control.Monad.Except (throwError)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API hiding (Metadata, TxBody)
import Shelley.Spec.Ledger.Tx
  ( decodeWits,
  )
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelocks
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Mary.Translation ()
import Control.Monad.Except (Except)

-- | Translate from Shelley to Mary by first translating
--   from Shelley to Allegra and then from Allegra to Mary.
shelleyToMary
  :: forall f c.
     ( TranslateEra (AllegraEra c) f
     , TranslateEra (MaryEra c) f
     , TranslationError (MaryEra c) f ~ TranslationError (AllegraEra c) f
     )
  => TranslationContext (MaryEra c)
  -> f (ShelleyEra c)
  -> Except (TranslationError (MaryEra c) f) (f (MaryEra c))
shelleyToMary _ state =
  translateEra @(AllegraEra c) () state >>= translateEra @(MaryEra c) ()

--------------------------------------------------------------------------------
-- Translate from Shelley to Voltaire prototype One.
--
-- Since the only difference between Voltaire prototype One and Mary is the era
-- name, we first translate from Shelley to Mary using 'shelleyToMary' and then
-- simply cast the result of this translation.
--------------------------------------------------------------------------------

type VoltaireOne c = One.VoltairePrototypeEra 'One.VoltairePrototype_One c

type instance TranslationContext (VoltaireOne c) = ()

instance Crypto c => TranslateEra (VoltaireOne c) NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes
        }

instance Crypto c => TranslateEra (VoltaireOne c) Tx where
  type TranslationError (VoltaireOne c) Tx = DecoderError
  translateEra _ctx tx = do
    tx' <- shelleyToMary _ctx tx
    case decodeAnnotator "tx" fromCBOR (serialize tx') of
      Right newTx -> pure newTx
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (VoltaireOne c) ShelleyGenesis where
  translateEra ctxt genesis =
    return
      ShelleyGenesis
        { sgSystemStart = sgSystemStart genesis,
          sgNetworkMagic = sgNetworkMagic genesis,
          sgNetworkId = sgNetworkId genesis,
          sgActiveSlotsCoeff = sgActiveSlotsCoeff genesis,
          sgSecurityParam = sgSecurityParam genesis,
          sgEpochLength = sgEpochLength genesis,
          sgSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
          sgMaxKESEvolutions = sgMaxKESEvolutions genesis,
          sgSlotLength = sgSlotLength genesis,
          sgUpdateQuorum = sgUpdateQuorum genesis,
          sgMaxLovelaceSupply = sgMaxLovelaceSupply genesis,
          sgProtocolParams = translateEra' ctxt (sgProtocolParams genesis),
          sgGenDelegs = sgGenDelegs genesis,
          sgInitialFunds = sgInitialFunds genesis,
          sgStaking = sgStaking genesis
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (VoltaireOne c) (PParams' f)

instance Crypto c => TranslateEra (VoltaireOne c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (VoltaireOne c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { _utxoState = translateEra' ctxt $ _utxoState ls,
          _delegationState = _delegationState ls
        }

instance Crypto c => TranslateEra (VoltaireOne c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (VoltaireOne c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (VoltaireOne c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { _utxo = translateEra' ctxt $ _utxo us,
          _deposited = _deposited us,
          _fees = _fees us,
          _ppups = translateEra' ctxt $ _ppups us
        }

instance Crypto c => TranslateEra (VoltaireOne c) TxOut where
  translateEra () txOut = do
    TxOutCompact addr cfval <- shelleyToMary () txOut
    pure $ TxOutCompact (coerce addr) cfval

instance Crypto c => TranslateEra (VoltaireOne c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO $ Map.map (translateEra' ctxt) $ unUTxO utxo

instance Crypto c => TranslateEra (VoltaireOne c) WitnessSet where
  type TranslationError (VoltaireOne c) WitnessSet = DecoderError
  translateEra _ctx ws = do
    ws' <- shelleyToMary _ctx ws
    case decodeAnnotator "witnessSet" decodeWits (serialize ws') of
      Right new -> pure new
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (VoltaireOne c) Update where
  translateEra ctxt update = do
    Update pp en <- shelleyToMary ctxt update
    pure $ Update (coerce pp) en

instance Crypto c => TranslateEra (VoltaireOne c) AuxiliaryData where
  translateEra _ (AuxiliaryData md as) =
    pure $ AuxiliaryData md (fmap Timelocks.translate as)
    -- For some reason GHC complains if we use 'shelleyToMary' to
    -- translate 'AuxiliaryData'. Therefore we use a dedicated
    -- function for this ('Timelocks.translate').