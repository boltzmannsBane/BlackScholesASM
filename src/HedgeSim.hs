module HedgeSim where

import Black76

type Price = Double
type PnL   = Double
type Time = Double

data HedgeState = HedgeState
  { hsCash   :: Double
  , hsDelta  :: Double
  , hsSpot   :: Double
  } deriving Show

stepHedge
  :: (Double -> Double -> Double -> Double) -- s -> t -> sigma -> delta
  -> Double   -- strike
  -> Double   -- rate
  -> Double   -- sigma
  -> [Price]
  -> [Time]
  -> [HedgeState]

stepHedge deltaFn k r sigma prices times =
  scanl go initState (zip prices times)
  where
    initDelta = deltaFn (head prices) (head times) sigma
    initState = HedgeState
      { hsCash  = -(initDelta * head prices)
      , hsDelta = initDelta
      , hsSpot  = head prices
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
