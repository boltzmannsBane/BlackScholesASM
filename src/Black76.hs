module Black76
  ( black76Call
  , black76Put
  , black76DeltaF
  , black76GammaF
  ) where

import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)

black76Call :: Double -> Double -> Double -> Double -> Double -> Double
black76Call f k t r sigma
    | t <= 0    = max 0 (f - k)
    | otherwise = exp (-r * t) * (f * normCdf d1 - k * normCdf d2)
  where
    denom = sigma * sqrt t
    d1 = (log (f / k) + 0.5 * sigma * sigma * t) / denom
    d2 = d1 - denom
    normCdf x = cumulative (normalDistr 0 1) x

black76Put :: Double -> Double -> Double -> Double -> Double -> Double
black76Put f k t r sigma
    | t <= 0    = max 0 (k - f)
    | otherwise = exp (-r * t) * (k * normCdf (-d2) - f * normCdf (-d1))
  where
    denom = sigma * sqrt t
    d1 = (log (f / k) + 0.5 * sigma * sigma * t) / denom
    d2 = d1 - denom
    normCdf x = cumulative (normalDistr 0 1) x

black76DeltaF :: Double -> Double -> Double -> Double -> Double -> Double
black76DeltaF f k t r sigma
  | t <= 0    = exp (-r * t) * (if f > k then 1 else 0)
  | otherwise = exp (-r * t) * normCdf d1
  where
    (d1, _) = d1d2 f k t sigma
    normCdf x = cumulative (normalDistr 0 1) x

black76GammaF :: Double -> Double -> Double -> Double -> Double -> Double
black76GammaF f k t r sigma
  | t <= 0    = 0
  | otherwise = exp (-r * t) * normPdf d1 / (f * sigma * sqrt t)
  where
    (d1, _) = d1d2 f k t sigma

-- Helpers

d1d2 :: Double -> Double -> Double -> Double -> (Double, Double)
d1d2 f k t sigma =
  let denom = sigma * sqrt t
      d1 = (log (f / k) + 0.5 * sigma * sigma * t) / denom
      d2 = d1 - denom
  in (d1, d2)

normPdf :: Double -> Double
normPdf x = exp (-0.5 * x * x) / sqrt (2 * pi)

-- ============================
-- Greeks
-- ============================

black76DeltaCall :: Double -> Double -> Double -> Double -> Double -> Double
black76DeltaCall f k r t sigma =
  let d1 = (log (f / k) + 0.5 * sigma * sigma * t) / (sigma * sqrt t)
  in exp (-r * t) * normCDF d1

black76DeltaPut :: Double -> Double -> Double -> Double -> Double -> Double
black76DeltaPut f k r t sigma =
  black76DeltaCall f k r t sigma - exp (-r * t)


-- ============================
-- Utilities
-- ============================

normCDF :: Double -> Double
normCDF x = 0.5 * (1.0 + erf (x / sqrt 2))

erf :: Double -> Double
erf z =
  let t = 1.0 / (1.0 + 0.5 * abs z)
      tau = t * exp (-z*z - 1.26551223 +
             t * (1.00002368 +
             t * (0.37409196 +
             t * (0.09678418 +
             t * (-0.18628806 +
             t * (0.27886807 +
             t * (-1.13520398 +
             t * (1.48851587 +
             t * (-0.82215223 +
             t * 0.17087277)))))))))
  in if z >= 0 then 1 - tau else tau - 1
