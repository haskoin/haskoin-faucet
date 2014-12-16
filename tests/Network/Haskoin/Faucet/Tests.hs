module Network.Haskoin.Faucet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, encode, decode)

import Network.Haskoin.Faucet.Arbitrary ()

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize JSON types"
        []
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

