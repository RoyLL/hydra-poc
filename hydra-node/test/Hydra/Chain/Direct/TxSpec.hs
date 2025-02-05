{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Chain.Direct.Tx
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tools as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (
  costModels,
  epochInfo,
  maxTxSize,
  pparams,
  systemStart,
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  adaOnly,
  genOneUTxOFor,
  genUTxOWithSimplifiedAddresses,
  hashTxOuts,
  shrinkUTxO,
 )
import Hydra.Party (Party, vkey)
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  conjoin,
  counterexample,
  cover,
  expectFailure,
  forAll,
  forAllShrinkBlind,
  label,
  oneof,
  property,
  vectorOf,
  withMaxSuccess,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  parallel $ do
    describe "collectComTx" $ do
      modifyMaxSuccess (const 10) $
        prop "validates" $ \headInput cperiod ->
          forAll (vectorOf 8 arbitrary) $ \parties ->
            forAll (generateCommitUTxOs parties) $ \commitsUTxO ->
              let onChainUTxO = UTxO $ Map.singleton headInput headOutput <> fmap fst commitsUTxO
                  headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
                  onChainParties = partyFromVerKey . vkey <$> parties
                  headDatum = Head.Initial cperiod onChainParties
                  tx =
                    collectComTx
                      testNetworkId
                      ( headInput
                      , headOutput
                      , fromPlutusData $ toData headDatum
                      , onChainParties
                      )
                      commitsUTxO
               in case validateTxScriptsUnlimited onChainUTxO tx of
                    Left basicFailure ->
                      property False & counterexample ("Basic failure: " <> show basicFailure)
                    Right redeemerReport ->
                      length commitsUTxO + 1 == length (rights $ Map.elems redeemerReport)
                        & counterexample (prettyRedeemerReport redeemerReport)
                        & counterexample ("Tx: " <> toString (renderTx tx))

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: UTxO -> TxIn -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum) (mkHeadTokenScript testSeedInput)
                headDatum = fromPlutusData $ toData Head.Closed{snapshotNumber = 1, utxoHash = ""}
                cbor = serialize tx
                len = LBS.length cbor
             in len < maxTxSize
                  & label (show (len `div` 1024) <> "KB")
                  & label (prettyLength utxo <> " entries")
                  & counterexample (toString (renderTx tx))
                  & counterexample ("Tx serialized size: " <> show len)
           where
            prettyLength :: Foldable f => f a -> String
            prettyLength (length -> len)
              | len >= 100 = "> 100"
              | len >= 50 = "50-99"
              | len >= 10 = "10-49"
              | otherwise = "00-10"

      -- FIXME: This property currently fails even with a single UTXO if this
      -- UTXO is generated with too many values. We need to deal with it eventually
      -- (fanout splitting) or find a better property to capture what is
      -- actually 'expectable' from the function, given arbitrary UTXO entries.
      prop "size is above limit for UTXO" $
        forAllShrinkBlind arbitrary shrinkUTxO $ \utxo ->
          forAll arbitrary $
            expectFailure . prop_fanoutTxSize utxo

      prop "validates" $ \headInput ->
        forAll (reasonablySized genUTxOWithSimplifiedAddresses) $ \inHeadUTxO ->
          let tx = fanoutTx inHeadUTxO (headInput, fromPlutusData $ toData headDatum) (mkHeadTokenScript testSeedInput)
              onChainUTxO = UTxO.singleton (headInput, headOutput)
              headScript = fromPlutusScript $ Head.validatorScript policyId
              -- FIXME: Ensure the headOutput contains enough value to fanout all inHeadUTxO
              headOutput =
                TxOut
                  (mkScriptAddress @PlutusScriptV1 testNetworkId headScript)
                  (lovelaceToValue $ Lovelace 10_000_000)
                  (toUTxOContext $ mkTxOutDatum headDatum)
              headDatum =
                Head.Closed
                  { snapshotNumber = 1
                  , utxoHash = toBuiltin (hashTxOuts $ toList inHeadUTxO)
                  }
           in checkCoverage $ case validateTxScriptsUnlimited onChainUTxO tx of
                Left basicFailure ->
                  property False & counterexample ("Basic failure: " <> show basicFailure)
                Right redeemerReport ->
                  conjoin
                    [ 1 == length (successfulRedeemersSpending redeemerReport)
                    , 1 == length (successfulRedeemersMinting redeemerReport)
                    ]
                    & label (show (length inHeadUTxO) <> " UTXO")
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample ("Tx: " <> show tx)
                    & cover 0.8 True "Success"

    describe "abortTx" $ do
      prop "validates" $
        \txIn contestationPeriod (ReasonablySized parties) -> forAll (genAbortableOutputs parties) $
          \(resolvedInitials, resolvedCommits) ->
            let headUTxO = (txIn :: TxIn, headOutput)
                headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
                headDatum =
                  Head.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
                initials = Map.fromList (drop2nd <$> resolvedInitials)
                initialsUTxO = drop3rd <$> resolvedInitials
                commits = Map.fromList (drop2nd <$> resolvedCommits)
                commitsUTxO = drop3rd <$> resolvedCommits
                utxo = UTxO $ Map.fromList (headUTxO : initialsUTxO <> commitsUTxO)
             in checkCoverage $ case abortTx testNetworkId (txIn, headOutput, fromPlutusData $ toData headDatum) initials (Map.fromList $ map tripleToPair resolvedCommits) of
                  Left OverlappingInputs ->
                    property (isJust $ txIn `Map.lookup` initials)
                  Right tx ->
                    case validateTxScriptsUnlimited utxo tx of
                      Left basicFailure ->
                        property False & counterexample ("Basic failure: " <> show basicFailure)
                      Right redeemerReport ->
                        1 + (length initials + length commits) == length (rights $ Map.elems redeemerReport)
                          & counterexample ("Redeemer report: " <> show redeemerReport)
                          & counterexample ("Tx: " <> toString (renderTx tx))
                          & counterexample ("Input utxo: " <> decodeUtf8 (encodePretty utxo))
                          & cover 0.8 True "Success"

      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) cardanoKeys walletUTxO ->
          let params = HeadParameters cperiod (party : parties)
              tx = initTx testNetworkId cardanoKeys params txIn
           in case observeInitTx testNetworkId party tx of
                Just (_, InitObservation{initials, threadOutput}) -> do
                  let (headInput, headOutput, headDatum, _) = threadOutput
                      initials' = Map.fromList [(a, c) | (a, _, c) <- initials]
                      lookupUTxO =
                        ((headInput, headOutput) : [(a, b) | (a, b, _) <- initials])
                          & Map.fromList
                          & Map.mapKeys toLedgerTxIn
                          & Map.map toLedgerTxOut
                   in case abortTx testNetworkId (headInput, headOutput, headDatum) initials' mempty of
                        Left err ->
                          property False & counterexample ("AbortTx construction failed: " <> show err)
                        Right (toLedgerTx -> txAbort) ->
                          case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO txAbort of
                            Left err ->
                              True
                                & label
                                  ( case err of
                                      ErrNoAvailableUTxO -> "No available UTxO"
                                      ErrNoPaymentUTxOFound -> "No payment UTxO found"
                                      ErrNotEnoughFunds{} -> "Not enough funds"
                                      ErrUnknownInput{} -> "Unknown input"
                                      ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                  )
                            Right (_, fromLedgerTx -> txAbortWithFees) ->
                              let actualExecutionCost = totalExecutionCost pparams txAbortWithFees
                                  fee = txFee' txAbortWithFees
                               in actualExecutionCost > Lovelace 0 && fee > actualExecutionCost
                                    & label "Ok"
                                    & counterexample ("Execution cost: " <> show actualExecutionCost)
                                    & counterexample ("Fee: " <> show fee)
                                    & counterexample ("Tx: " <> show txAbortWithFees)
                                    & counterexample ("Input utxo: " <> show (walletUTxO <> lookupUTxO))
                _ ->
                  property False
                    & counterexample "Failed to construct and observe init tx."
                    & counterexample (toString (renderTx tx))

mkInitials ::
  (TxIn, Hash PaymentKey, TxOut CtxUTxO) ->
  UTxOWithScript
mkInitials (txin, vkh, TxOut _ value _) =
  let initDatum = Initial.datum (toPlutusKeyHash vkh)
   in ( txin
      , mkInitialTxOut vkh value
      , fromPlutusData (toData initDatum)
      )

mkInitialTxOut ::
  Hash PaymentKey ->
  Value ->
  TxOut CtxUTxO
mkInitialTxOut vkh initialValue =
  toUTxOContext $
    TxOut
      (mkScriptAddress @PlutusScriptV1 testNetworkId initialScript)
      initialValue
      (mkTxOutDatum initialDatum)
 where
  initialScript = fromPlutusScript Initial.validatorScript
  initialDatum = Initial.datum (toPlutusKeyHash vkh)

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, ScriptData))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  committedUTxO <-
    vectorOf (length parties) $
      oneof
        [ do
            singleUTxO <- fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
            pure $ head <$> nonEmpty (UTxO.pairs singleUTxO)
        , pure Nothing
        ]
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip parties committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: Party -> Maybe (TxIn, TxOut CtxUTxO) -> (TxOut CtxUTxO, ScriptData)
  mkCommitUTxO party utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV1 testNetworkId commitScript)
          -- TODO(AB): We should add the value from the initialIn too because it contains
          -- the PTs
          commitValue
          (mkTxOutDatum commitDatum)
    , fromPlutusData (toData commitDatum)
    )
   where
    commitValue = lovelaceToValue (Lovelace 2000000) <> maybe mempty (txOutValue . snd) utxo
    commitScript = fromPlutusScript Commit.validatorScript
    commitDatum = mkCommitDatum party (Head.validatorHash policyId) utxo

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
--
-- TODO: We may want to define a full cardano-api version of that one somewhere
-- else (that is, which also return cardano-api types and not ledger ones).
validateTxScriptsUnlimited ::
  -- | UTxO set used to create context for any tx inputs.
  UTxO ->
  Tx ->
  Either
    (Ledger.BasicFailure StandardCrypto)
    (Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits))
validateTxScriptsUnlimited (toLedgerUTxO -> utxo) (toLedgerTx -> tx) =
  runIdentity $
    Ledger.evaluateTransactionExecutionUnits
      pparams
      tx
      utxo
      epochInfo
      systemStart
      costModels

prettyRedeemerReport ::
  Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits) ->
  String
prettyRedeemerReport (Map.toList -> xs) =
  "Script Evaluation(s):\n" <> intercalate "\n" (prettyKeyValue <$> xs)
 where
  prettyKeyValue (ptr, result) =
    toString ("  - " <> show ptr <> ": " <> prettyResult result)
  prettyResult =
    either (T.replace "\n" " " . show) show

successfulRedeemersSpending ::
  Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits) ->
  [Ledger.RdmrPtr]
successfulRedeemersSpending =
  Map.foldrWithKey onlySuccessfulSpending []
 where
  onlySuccessfulSpending ptr = \case
    Left{} -> identity
    Right{} ->
      case ptr of
        Ledger.RdmrPtr Ledger.Spend _ -> (ptr :)
        Ledger.RdmrPtr _ _ -> identity

successfulRedeemersMinting ::
  Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits) ->
  [Ledger.RdmrPtr]
successfulRedeemersMinting =
  Map.foldrWithKey onlySuccessfulMinting []
 where
  onlySuccessfulMinting ptr = \case
    Left{} -> identity
    Right{} ->
      case ptr of
        Ledger.RdmrPtr Ledger.Mint _ -> (ptr :)
        Ledger.RdmrPtr _ _ -> identity

genAbortableOutputs :: [Party] -> Gen ([UTxOWithScript], [UTxOWithScript])
genAbortableOutputs parties = do
  (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
  initials <- vectorOf (length initParties) genInitial
  commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
  pure (initials, commits)

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _, c) = (a, c)

drop3rd :: (a, b, c) -> (a, b)
drop3rd (a, b, _) = (a, b)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))

genInitial :: Gen UTxOWithScript
genInitial = mkInitials <$> arbitrary
