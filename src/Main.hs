module Main where

import HedgeSim
import Black76
import System.IO

main :: IO ()
main = do
  let prices = [100,101,99,102,105,103,104,106,108,110]
      times  = reverse [0.1,0.2..1.0]
      k      = 100
      r      = 0.01
      
      sigma = 0.2

      deltaFn s t = black76DeltaCall s k r t

      states = stepHedge deltaFn k r prices times

      finalSpot = last prices
      payoff = payoffCall k finalSpot

      pnls = map (\st -> computePnL st payoff) states

  writeFile "pnl.csv" $
    unlines $ zipWith (\i p -> show i ++ "," ++ show p) [0..] pnls

  print $ last pnls

import MojoBridge

runMojo :: IO ()
runMojo = do
  let input = MojoInput
        { s0 = 100
        , mu = 0
        , sigma = 0.2
        , r = 0.01
        , k = 100
        , steps = 252
        , paths = 100000
        }

  result <- runMojoMC input
  print result
