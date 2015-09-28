# Haskoin Faucet

Simple faucet implementation running on top of haskoin-wallet.

## Features

This package provides a simple faucet implementation written with Yesod on top
of the haskoin-wallet package.

## Installing

Get dependencies:

- [Stack](https://github.com/commercialhaskell/stack)
- [PCRE](http://pcre.org/)
- [ØMQ](http://zeromq.org/)
- [Git](http://git-scm.com/)
- [LevelDB](https://github.com/google/leveldb)
- [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config/)
- [zlib](http://zlib.net/)

Ubuntu:

```sh
sudo apt-get install -y git libleveldb-dev libzmq3-dev \
    pkg-config zlib1g-dev libpcre3-dev
```

Clone repository and build:

```sh
git clone https://github.com/haskoin/haskoin-faucet.git
cd haskoin-faucet
stack build
```

## Running

These examples are for testnet in a Linux system. Run from directory where
repository was cloned.

Start an instance of Haskoin Wallet in the background:

```sh
stack exec hw -- -w .hw -t -d start
```

Create a keyring:

```sh
stack exec hw -- -w .hw -t newkeyring
```

Create a `faucet` account:

```sh
stack exec hw -- -w .hw -t newacc faucet
```

Start the faucet:

```sh
HW_SOCKET="ipc://.hw/testnet/hw.sock" \
    stack exec haskoin-faucet -- config/settings.yml
```

The faucet will be accessible at [this URL](http://localhost:54705).

## Contributing

Feel free to send us your pull request.
