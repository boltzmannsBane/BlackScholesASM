module MojoBridgeSpec where

import Test.Tasty
import Test.Tasty.HUnit
import MojoBridge

tests :: TestTree
tests = testGroup "MojoBridge"
  [ testCase "Mojo roundtrip produces sane output" $ do
      let input = MojoInput
            { s0 = 100
            , mu = 0
            , sigma = 0.2
            , r = 0.01
            , k = 100
            , steps = 10
            , paths = 1000
            }

      out <- runMojoMC input

      assertBool "mean finite" (not $ isNaN (mean out))
      assertBool "std positive" (std out >= 0)
      assertBool "var <= cvar" (var_99 out <= cvar_99 out)
  ]
