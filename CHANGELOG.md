# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [0.4.0] - UNRELEASED

## Fixed

- `hydra-tui` to show form focus, indicate invalid fields in dialogs and only allow valid values
  to be submitted [#224](https://github.com/input-output-hk/hydra-poc/issues/224).

## [0.3.0] - 2022-02-22

#### Added

- Implementation of on-chain verification of Hydra Head lifecycle without contests. This first version with its various shortcuts is documented on examples of the [full](./docs/images/on-chain-full.jpg) and [abort](./docs/images/on-chain-abort.jpg) on-chain life-cycles of a Hydra Head
- Enable nix-shell on Mac
- Build separate docker images for `hydra-node` and `hydra-tui` available as [packages](https://github.com/orgs/input-output-hk/packages?repo_name=hydra-poc) from GitHub repo
- Utility executable `inspect-script` to dump contracts for further analysis
- CBOR encoder and Merkle-Tree in Plutus as separate packages `plutus-cbor` and `plutus-merkle-tree`, released & tagged separately

#### Changed

- Package `local-cluster` is now `hydra-cluster`.
- Use `cardano-api` types and functions to interact with chain.
- Refine computation of fees from internal wallet.
- Remove several sources of `error` in chain interaction component.

#### Known issues

- `collectComTx` requires increase in tx size limit over current mainchain parameters to 32KB, which should be alleviated with Plutus optimisations and merging all contracts in one in future releases
- Head can collect at most 9 commits and each party can commit either 1 or 0 UTXO to a Head
- `fanoutTx` cannot handle more than 100 UTxO with the standard tx size of 16KB (200 with the temporary increase for test purpose).
- Known issues from `0.2.0` still apply

## [0.2.0] - 2021-12-14

#### Added
- Direct chain integration which allows to connect to a real cardano-node /
  devnet; no on-chain validators though.
- Support alonzo transactions inside the Hydra Head. For now using a `freeCostModel`.
- Command line options `--node-socket`, `--network-magic` and
  `--cardano-{signing,verification}-key` to `hydra-node` and `hydra-tui` to
  configure the Cardano network access.

#### Changed
- Command line options of `hydra-node` quite significantly to distinguish hydra
  credentials from cardano credentials.
- Commit and transaction creation logic of TUI to use cardano credentials.

#### Removed
- ZeroMQ mock-chain executable, chain component and corresponding `hydra-node`
  command line options.
- ZeroMQ based network component.
- Aliases from party identifiers.

#### Fixed
- `hydra-tui` to correctly show current state when re-connecting.

#### Known issues
- There can only be one Head per Cardano network (i.e. on the devnet).
- Only no or one utxo can be committed to a Head.
- Recipient addresses to send money to in the TUI are inferred from the current
  UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a
  Head, it won't be able to send or receive anything anymore.
- TUI crashes when user tries to post a new transaction wihout any UTXO
  remaining.
- Not an issue, but a workaround: The internal wallet of `hydra-node` requires a
  UTXO to be marked as "fuel" to drive the Hydra protocol transactions.

## [0.1.0] - 2021-09-30

- First proof-of-concept for a `hydra-node`

### Added
- Coordinated Hydra Head protocol
- Single Head per hydra-node
- Stubbed chain using external process
- Network statically configured, direct TCP connections
- WebSocket, message-based API Server
- Terminal user interface client
