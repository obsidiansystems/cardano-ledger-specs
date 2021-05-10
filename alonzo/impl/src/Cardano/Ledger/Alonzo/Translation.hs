{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Binary
  ( Annotator,
    DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    serialize,
  )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..), PParamsUpdate)
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits, Prices)
import Cardano.Ledger.Alonzo.Tx (IsValidating (..), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Tx as LTX
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
  ( EpochState (..),
    NewEpochState (..),
    ShelleyGenesis,
    StrictMaybe (..),
  )
import qualified Shelley.Spec.Ledger.API as API
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

import qualified Data.Map.Strict as Map
import Cardano.Ledger.Val
import Cardano.Ledger.Coin
import Data.Default.Class
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade(..), emptySnapShots)
import qualified Cardano.Ledger.Crypto as CC

--------------------------------------------------------------------------------
-- Translation from Mary to Alonzo
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

data AlonzoGenesis = AlonzoGenesis
  { adaPerUTxOWord :: API.Coin,
    costmdls :: Map Language CostModel,
    prices :: Prices,
    maxTxExUnits :: ExUnits,
    maxBlockExUnits :: ExUnits,
    maxValSize :: Natural
  }
  deriving (Eq)

type instance PreviousEra (AlonzoEra c) = MaryEra c

type instance TranslationContext (AlonzoEra c) = AlonzoGenesis

instance
  (Crypto c) =>
  TranslateEra (AlonzoEra c) NewEpochState
  where
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

instance Crypto c => TranslateEra (AlonzoEra c) Core.Tx where
  type TranslationError (AlonzoEra c) Core.Tx = DecoderError
  translateEra _ctx tx =
    case decodeAnnotator "tx" fromCBOR (serialize tx) of
      Right newTx -> pure newTx
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (AlonzoEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    return
      API.ShelleyGenesis
        { API.sgSystemStart = API.sgSystemStart genesis,
          API.sgNetworkMagic = API.sgNetworkMagic genesis,
          API.sgNetworkId = API.sgNetworkId genesis,
          API.sgActiveSlotsCoeff = API.sgActiveSlotsCoeff genesis,
          API.sgSecurityParam = API.sgSecurityParam genesis,
          API.sgEpochLength = API.sgEpochLength genesis,
          API.sgSlotsPerKESPeriod = API.sgSlotsPerKESPeriod genesis,
          API.sgMaxKESEvolutions = API.sgMaxKESEvolutions genesis,
          API.sgSlotLength = API.sgSlotLength genesis,
          API.sgUpdateQuorum = API.sgUpdateQuorum genesis,
          API.sgMaxLovelaceSupply = API.sgMaxLovelaceSupply genesis,
          API.sgProtocolParams = translateEra' ctxt (API.sgProtocolParams genesis),
          API.sgGenDelegs = API.sgGenDelegs genesis,
          API.sgInitialFunds = API.sgInitialFunds genesis,
          API.sgStaking = API.sgStaking genesis
        }

newtype TxInBlock era = TxInBlock (Era.TxInBlock era)

instance
  ( Crypto c,
    Era.TxInBlock (AlonzoEra c) ~ ValidatedTx (AlonzoEra c)
  ) =>
  TranslateEra (AlonzoEra c) TxInBlock
  where
  type TranslationError (AlonzoEra c) TxInBlock = DecoderError
  translateEra _ctxt (TxInBlock tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    bdy <- translateViaCBORAnn "txbody" $ LTX.body tx
    txwits <- translateViaCBORAnn "txwitness" $ LTX.wits tx
    -- transactions from Mary era always pass script ("phase 2") validation
    aux <- case LTX.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust axd -> SJust <$> translateViaCBORAnn "auxiliarydata" axd
    let validating = IsValidating True
    pure $ TxInBlock $ ValidatedTx bdy txwits validating aux

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

translateViaCBORAnn :: (ToCBOR a, FromCBOR (Annotator b)) => Text -> a -> Except DecoderError b
translateViaCBORAnn name x =
  case decodeAnnotator name fromCBOR (serialize x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError

instance (Crypto c, Functor f) => TranslateEra (AlonzoEra c) (API.PParams' f)

instance Crypto c => TranslateEra (AlonzoEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translatePParams ctxt $ esPrevPp es,
          esPp = translatePParams ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.LedgerState where
  translateEra ctxt ls =
    return
      API.LedgerState
        { API._utxoState = translateEra' ctxt $ API._utxoState ls,
          API._delegationState = API._delegationState ls
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxOState where
  translateEra ctxt us =
    return
      API.UTxOState
        { API._utxo = translateEra' ctxt $ API._utxo us,
          API._deposited = API._deposited us,
          API._fees = API._fees us,
          API._ppups = translateEra' ctxt $ API._ppups us
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxO where
  translateEra _ctxt utxo =
    return $ API.UTxO $ fmap translateTxOut $ API.unUTxO utxo

instance Crypto c => TranslateEra (AlonzoEra c) API.PPUPState where
  translateEra ctxt ps =
    return
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    return $ API.ProposedPPUpdates $ fmap translatePParamsUpdate ppup

translateTxOut ::
  Core.TxOut (MaryEra c) -> Core.TxOut (AlonzoEra c)
translateTxOut (Shelley.TxOutCompact addr value) =
  TxOutCompact addr value SNothing

translatePParams ::
  AlonzoGenesis -> Shelley.PParams maryEra -> PParams alonzoEra
translatePParams ctx pp =
  PParams
    { _minfeeA = API._minfeeA pp,
      _minfeeB = API._minfeeB pp,
      _maxBBSize = API._maxBBSize pp,
      _maxTxSize = API._maxTxSize pp,
      _maxBHSize = API._maxBHSize pp,
      _keyDeposit = API._keyDeposit pp,
      _poolDeposit = API._poolDeposit pp,
      _eMax = API._eMax pp,
      _nOpt = API._nOpt pp,
      _a0 = API._a0 pp,
      _rho = API._rho pp,
      _tau = API._tau pp,
      _d = API._d pp,
      _extraEntropy = API._extraEntropy pp,
      _protocolVersion = API._protocolVersion pp,
      _minPoolCost = API._minPoolCost pp,
      --added in Alonzo
      _adaPerUTxOWord = adaPerUTxOWord ctx,
      _costmdls = costmdls ctx,
      _prices = prices ctx,
      _maxTxExUnits = maxTxExUnits ctx,
      _maxBlockExUnits = maxBlockExUnits ctx,
      _maxValSize = maxValSize ctx
    }

translatePParamsUpdate ::
  Shelley.PParamsUpdate (MaryEra c) -> PParamsUpdate (AlonzoEra c)
translatePParamsUpdate pp =
  PParams
    { _minfeeA = API._minfeeA pp,
      _minfeeB = API._minfeeB pp,
      _maxBBSize = API._maxBBSize pp,
      _maxTxSize = API._maxTxSize pp,
      _maxBHSize = API._maxBHSize pp,
      _keyDeposit = API._keyDeposit pp,
      _poolDeposit = API._poolDeposit pp,
      _eMax = API._eMax pp,
      _nOpt = API._nOpt pp,
      _a0 = API._a0 pp,
      _rho = API._rho pp,
      _tau = API._tau pp,
      _d = API._d pp,
      _extraEntropy = API._extraEntropy pp,
      _protocolVersion = API._protocolVersion pp,
      _minPoolCost = API._minPoolCost pp,
      --added in Alonzo
      _adaPerUTxOWord = SNothing,
      _costmdls = SNothing,
      _prices = SNothing,
      _maxTxExUnits = SNothing,
      _maxBlockExUnits = SNothing,
      _maxValSize = SNothing
    }

instance forall c.
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (AlonzoEra c)
  where
  type AdditionalGenesisConfig (AlonzoEra c) = AlonzoGenesis
  -- initialState :: ShelleyGenesis era -> AdditionalGenesisConfig era -> NewEpochState era
  initialState sg ctx =
    API.NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( API.EpochState
          (API.AccountState (Coin 0) reserves)
          emptySnapShots
          ( API.LedgerState
              ( API.UTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
              )
              (API.DPState (def {API._genDelegs = API.GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (API.PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = API.genesisUTxO sg
      reserves =
        coin $
          inject (word64ToCoin (API.sgMaxLovelaceSupply sg))
            <-> API.balance initialUtxo
      genDelegs = API.sgGenDelegs sg
      pp = translatePParams ctx $ API.sgProtocolParams sg

instance API.PraosCrypto c => API.ShelleyBasedEra (AlonzoEra c)

