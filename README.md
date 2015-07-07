# Haskoin Faucet

Simple faucet implementation running on top of haskoin-wallet.

## Features

This package provides a simple faucet implementation written with Yesod on top
of the haskoin-wallet package. 

## Installing

Get dependencies:

- [Stack](https://github.com/commercialhaskell/stack)
- [MySQL](http://dev.mysql.com/)
- [PCRE](http://pcre.org/)
- [ØMQ](http://zeromq.org/)
- [Git](http://git-scm.com/)
- [LevelDB](https://github.com/google/leveldb)
- [Snappy](https://code.google.com/p/snappy/)
- [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config/)
- [zlib](http://zlib.net/)

Ubuntu:

```sh
sudo apt-get install -y git libleveldb-dev \
    libzmq3-dev libsnappy-dev pkg-config zlib1g-dev \
    libmysqlclient-dev libpcre3-dev
```

Clone repository and install:

```sh
git clone https://github.com/haskoin/haskoin-faucet.git
cd haskoin-faucet
stack install
```

## Running

Create database in MySQL server.

Start an instance of Haskoin Wallet:

```sh
# Example for testnet
~/.local/bin/hw start -t -d
```

Copy `settings.yml` to a location of your choice, and edit it to match your
setup. Alternatively use environment variables as shown in the same file.

Copy the `static` directory from the source distribution into your faucet
directory.

Start the faucet from the faucet directory:

```sh
~/.local/bin/haskoin-faucet /location/of/settings.yml
```

## Contributing

Commits are done through GitHub pull requests.
