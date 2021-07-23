{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

module Hydra.ContractTest where

import Hydra.Prelude

import Ledger

import Hydra.Test.Utils (
  assertFinalState,
  callEndpoint,
  prettyUtxo,
  utxoOf,
  vk,
 )
import Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Ledger.Value (valueOf)
import Plutus.Contract (Contract)
import Plutus.Contract.Test (
  Wallet (..),
  assertNoFailedTransactions,
  checkPredicate,
  walletFundsChange,
  (.&&.),
 )
import Plutus.PAB.Arbitrary ()
import PlutusTx.Monoid (inv)
import Test.Tasty (TestTree, testGroup)
import Wallet.Types (ContractError (..))

import qualified Control.Monad.Freer.Extras.Log as Trace
import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OffChain as OffChain
import qualified Hydra.Contract.OnChain as OnChain
import Hydra.Test.Utils (assertContractFailed)
import qualified Plutus.Trace.Emulator as Trace
import Test.QuickCheck (Gen, choose)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)
import qualified Prelude

--
-- Fixture
--

alice :: Wallet
alice = Wallet 1

bob :: Wallet
bob = Wallet 2

testPolicy :: MintingPolicy
testPolicy = OnChain.hydraMintingPolicy 42

testPolicyId :: MintingPolicyHash
testPolicyId = mintingPolicyHash testPolicy

contract :: Contract [OnChain.State] OffChain.Schema ContractError ()
contract = OffChain.contract headParameters
 where
  headParameters = OffChain.mkHeadParameters [vk alice, vk bob] testPolicy

--
-- Helpers
--

-- | Generate value-preserving Utxo from seed and existing Utxo
generateUtxo :: [TxOut] -> Gen [TxOut]
generateUtxo initialUtxo =
  go totalAmount
 where
  totalAmount = valueOf (foldMap txOutValue initialUtxo) adaSymbol adaToken

  go :: Integer -> Gen [TxOut]
  go 0 = pure []
  go amount = do
    addr <- arbitrary
    val <- choose (1, amount)
    let txOut = TxOut addr (lovelaceValueOf val) Nothing
    (txOut :) <$> go (amount - val)

generateWith :: Gen a -> Int -> a
generateWith (MkGen run) seed = run (mkQCGen seed) 30

--
-- Test
--

tests :: TestTree
tests =
  testGroup
    "Hydra Scenarios"
    [ checkPredicate
        "Init > Commit > Commit > CollectCom > Close: Can Close an opened head"
        ( assertNoFailedTransactions
            .&&. assertFinalState contract alice stateIsClosed
            .&&. walletFundsChange alice (inv fixtureAmount)
            .&&. walletFundsChange bob (inv fixtureAmount)
        )
        $ do
          aliceH <- setupWallet alice
          bobH <- setupWallet bob

          callEndpoint @"init" aliceH ()

          utxoAlice <- utxoOf alice
          Trace.logInfo ("Alice's UTxO: " <> prettyUtxo utxoAlice)
          let aliceCommit = selectOne utxoAlice
          callEndpoint @"commit" aliceH (vk alice, aliceCommit)

          utxoBob <- utxoOf bob
          Trace.logInfo ("Bob's UTxO: " <> prettyUtxo utxoBob)
          let bobCommit = selectOne utxoBob
          callEndpoint @"commit" bobH (vk bob, bobCommit)

          let committedUtxo = txOutTxOut . snd <$> [aliceCommit, bobCommit]

          callEndpoint @"collectCom" aliceH (vk alice, committedUtxo)

          -- TODO generate arbitrary snapshots without inflight txs
          let snapshot = OnChain.Snapshot 1 utxo
              utxo = generateWith (generateUtxo committedUtxo) 42

          callEndpoint @"close" aliceH (vk alice, snapshot)
    , checkPredicate
        "Init > Abort: One can always abort before head is open"
        ( assertNoFailedTransactions
            .&&. assertFinalState contract alice stateIsFinal
            .&&. walletFundsChange alice (lovelaceValueOf 0)
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          callEndpoint @"abort" aliceH (vk alice, [])
    , checkPredicate
        "Init > Commit > Abort: One can always abort before head is open"
        ( assertNoFailedTransactions
            .&&. assertFinalState contract alice stateIsFinal
            .&&. walletFundsChange alice (lovelaceValueOf 0)
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          utxoAlice <- selectOne <$> utxoOf alice
          callEndpoint @"commit" aliceH (vk alice, utxoAlice)
          callEndpoint @"abort" aliceH (vk alice, [txOutTxOut $ snd utxoAlice])
    , checkPredicate
        "Init > Commit > CollectCom: CollectCom is not allowed when not all parties have committed"
        ( assertFinalState contract alice stateIsInitial
            .&&. walletFundsChange alice (inv fixtureAmount)
            .&&. assertContractFailed contract alice "CollectCom"
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          utxoAlice <- selectOne <$> utxoOf alice
          callEndpoint @"commit" aliceH (vk alice, utxoAlice)
          callEndpoint @"collectCom" aliceH (vk alice, [])
    ]

fixtureAmount :: Value
fixtureAmount = lovelaceValueOf 1000

stateIsInitial :: OnChain.State -> Bool
stateIsInitial = \case
  OnChain.Initial{} -> True
  _ -> False

stateIsOpen :: OnChain.State -> Bool
stateIsOpen = \case
  OnChain.Open{} -> True
  _ -> False

stateIsClosed :: OnChain.State -> Bool
stateIsClosed = \case
  OnChain.Closed{} -> True
  _ -> False

hasTwoTxOuts :: OnChain.State -> Bool
hasTwoTxOuts = \case
  OnChain.Open committedOutputs -> length committedOutputs == 2
  _ -> False

stateIsFinal :: OnChain.State -> Bool
stateIsFinal = \case
  OnChain.Final{} -> True
  _ -> False

selectOne :: UtxoMap -> (TxOutRef, TxOutTx)
selectOne =
  Prelude.head
    . Map.toList
    . Map.filter ((== fixtureAmount) . txOutValue . txOutTxOut)

setupWallet ::
  Wallet ->
  Trace.EmulatorTrace (Trace.ContractHandle [OnChain.State] OffChain.Schema ContractError)
setupWallet user = do
  h <- Trace.activateContractWallet user contract
  Trace.callEndpoint @"setupForTesting" h (vk user, replicate 10 fixtureAmount)
  void Trace.nextSlot
  return h
