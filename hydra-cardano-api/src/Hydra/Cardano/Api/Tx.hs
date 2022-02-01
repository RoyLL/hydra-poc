module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.KeyWitness (toLedgerBootstrapWitness, toLedgerKeyWitness)
import Hydra.Cardano.Api.Lovelace (fromLedgerCoin)
import Hydra.Cardano.Api.TxScriptValidity (toLedgerScriptValidity)

import qualified Cardano.Ledger.Alonzo as Ledger
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Era as Ledger
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)

-- * Extra

-- | Get explicit fees allocated to a transaction.
--
-- NOTE: this function is partial and throws if given a Byron transaction for
-- which fees are necessarily implicit.
txFee' :: Tx Era -> Lovelace
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit TxFeesExplicitInAlonzoEra fee -> fee
    TxFeeImplicit _ -> error "impossible: TxFeeImplicit on non-Byron transaction."

{-# DEPRECATED getFee "use txFee' instead." #-}
getFee :: Tx Era -> Lovelace
getFee = txFee'

-- | Calculate the total execution cost of a transaction, according to the
-- budget assigned to each redeemer.
totalExecutionCost ::
  Ledger.PParams LedgerEra ->
  Tx Era ->
  Lovelace
totalExecutionCost pparams tx =
  fromLedgerCoin (Ledger.txscriptfee (Ledger._prices pparams) executionUnits)
 where
  executionUnits = foldMap snd $ Ledger.unRedeemers $ Ledger.txrdmrs wits
  Ledger.ValidatedTx{Ledger.wits = wits} = toLedgerTx tx

executionCost ::
  Ledger.PParams LedgerEra ->
  Tx Era ->
  Lovelace
executionCost = totalExecutionCost
{-# DEPRECATED executionCost "use totalExecutionCost instead." #-}

-- * Type Conversions

toLedgerTx :: Tx Era -> Ledger.ValidatedTx LedgerEra
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Redeemers mempty)
     in Ledger.ValidatedTx
          { Ledger.body =
              body
          , Ledger.isValid =
              toLedgerScriptValidity validity
          , Ledger.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.wits =
              Ledger.TxWitness
                { Ledger.txwitsVKey =
                    toLedgerKeyWitness vkWits
                , Ledger.txwitsBoot =
                    toLedgerBootstrapWitness vkWits
                , Ledger.txscripts =
                    fromList
                      [ ( Ledger.hashScript @LedgerEra s
                        , s
                        )
                      | s <- scripts
                      ]
                , Ledger.txdats =
                    datums
                , Ledger.txrdmrs =
                    redeemers
                }
          }
