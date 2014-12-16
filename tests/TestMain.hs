module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Faucet.Units (tests)
import qualified Network.Haskoin.Faucet.Tests (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Faucet.Tests.tests
    ++ Network.Haskoin.Faucet.Units.tests 
    )

