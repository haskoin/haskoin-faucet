# Haskoin Faucet

Simple faucet implementation running on top of haskoin-wallet.

## Features

This package provides a simple faucet implementation written with Yesod on top
of the haskoin-wallet package. 

## Installing

### Dependencies

This package depends on haskoin and haskoin-wallet.  First, install them from
their GitHub repositories.  It is recommended to install Haskoin Wallet with
MySQL support:

- https://github.com/haskoin/haskoin
- https://github.com/haskoin/haskoin-wallet

If MySQL support not added already to Haskoin Wallet, install MySQL client
development libraries.  On Debian/Ubuntu:

```sh
sudo apt-get install libmysqlclient-dev
```

Install MySQL.  On Debian/Ubuntu:

```sh
sudo apt-get install mysql-server
```

Create database:

```sh
mysql -u root -p
> create database haskoin_faucet
```

### Git

Install from Git:

```sh
git clone https://github.com/haskoin/haskoin-faucet.git
cd haskoin-faucet
cabal install
```

## Running

You need runnning instances of MySQL database, and Haskoin Wallet.  Run Haskoin
Wallet with minimum-confirmations parameter set to zero.  You can set this
either from server.yml or the MINCONF environment variable.

To configure haskoin-faucet, either change configuration file in
config/settings.yml, or set these environment variables before running
haskoin-faucet:

- HOST
- PORT
- APPROOT (ie. http://faucet.example.com)
- IP\_FROM\_HEADER
- WITHDRAW\_LIMIT (amount to withdraw per transaction)
- RESET\_TIME (time to withdraw again)
- HW\_SOCKET (ie. ipc:///home/haskoin/.hw/testnet/hw-spv)
- WALLET (Haskoin wallet name where faucet account is)
- ACCOUNT (Haskoin account to withdraw funds from)
- MYSQL\_USER
- MYSQL\_PASSWORD
- MYSQL\_HOST
- MYSQL\_PORT
- MYSQL\_DATABASE

If running Haskoin Faucet from a non-root user, you may want to setup a reverse
proxy or port redirection.  That is not explained here.

## Contributing

Commits are done through GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.
