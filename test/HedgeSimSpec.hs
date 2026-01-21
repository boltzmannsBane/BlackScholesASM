module HedgeSimSpec where

import Test.Tasty
import Test.Tasty.HUnit
import HedgeSim

tests :: TestTree
tests = testGroup "HedgeSim"
  [ testCase "Zero move, zero sigma -> zero PnL" $
      let prices = replicate 10 100
          times  = reverse [0.1,0.2..1.0]
          k = 100
          r = 0.0
          sigma = 0.0
          deltaFn _ _ _ = 0.0
          states = stepHedge deltaFn k r sigma prices times
          payoff = 0
          pnls = map (`computePnL` payoff) states
      in last pnls @?= 0.0
  ]
