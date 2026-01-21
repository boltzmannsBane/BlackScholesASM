module HedgeSim where

import Black76

type Price = Double
type Time  = Double

data HedgeState = HedgeState
  { hsCash  :: Double
  , hsDelta :: Double
  , hsSpot  :: Double
  } deriving Show

stepHedge
  :: (Double -> Double -> Double -> Double)
  -> Double
  -> Double
  -> Double
  -> [Price]
  -> [Time]
  -> [HedgeState]
stepHedge _ _ _ _ [] _ = []
stepHedge _ _ _ _ _ [] = []
stepHedge deltaFn _ _ sigma (p0:ps) (t0:ts) =
  scanl go initState (zip ps ts)
  where
    initDelta = deltaFn p0 t0 sigma
    initState = HedgeState
      { hsCash  = -(initDelta * p0)
      , hsDelta = initDelta
      , hsSpot  = p0
      }

    go st (s, t) =
      let newDelta = deltaFn s t sigma
          dDelta   = newDelta - hsDelta st
          cash'    = hsCash st - dDelta * s
      in HedgeState cash' newDelta s

computePnL :: HedgeState -> Double -> Double
computePnL st payoff =
  hsCash st + hsDelta st * hsSpot st - payoff

payoffCall :: Double -> Double -> Double
payoffCall k s = max 0 (s - k)

