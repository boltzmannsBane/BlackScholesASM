module Main where

import Test.Tasty
import qualified HedgeSimSpec
import qualified MojoBridgeSpec

main :: IO ()
main = defaultMain $
  testGroup "All Tests"
    [ HedgeSimSpec.tests
    , MojoBridgeSpec.tests
    ]
