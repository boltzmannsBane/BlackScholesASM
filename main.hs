{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative (optional)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Map.Strict as M
import Data.Time (Day, defaultTimeLocale, parseTimeM, fromGregorian, diffDays)
import qualified Data.Vector as V
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)
import Text.Printf (printf)

-- ==========================================
-- 1. Configuration
-- ==========================================
strikePrice  :: Double
riskFreeRate :: Double
volatility   :: Double
expiryDate   :: Day

strikePrice   = 75.00
riskFreeRate  = 0.04
volatility    = 0.35
expiryDate    = fromGregorian 2026 12 31

-- ==========================================
-- 2. Parsing (Backward Compatible)
-- ==========================================

-- Spot Data (YYYY-MM-DD)
data SpotRecord = SpotRecord { sDate :: Day, sPrice :: Maybe Double }

instance FromNamedRecord SpotRecord where
  parseNamedRecord r = do
    -- request the Date field as a String
    dateStr <- r .: "Date"
    day <- case (parseTimeM True defaultTimeLocale "%Y-%m-%d" (dateStr :: String) :: Maybe Day) of
             Just d  -> pure d
             Nothing -> fail $ "Invalid spot date: " ++ dateStr
    price <- optional (r .: "Price")
    pure $ SpotRecord day price

-- Futures Data (MM/DD/YYYY)
data FutRecord = FutRecord { fDate :: Day, fPrice :: Maybe Double }

instance FromNamedRecord FutRecord where
  parseNamedRecord r = do
    dateStr <- r .: "Date"
    day <- case (parseTimeM True defaultTimeLocale "%m/%d/%Y" (dateStr :: String) :: Maybe Day) of
             Just d  -> pure d
             Nothing -> fail $ "Invalid futures date: " ++ dateStr
    price <- optional (r .: "Price")
    pure $ FutRecord day price

-- ==========================================
-- 3. The Two Models
-- ==========================================

-- Black-Scholes (For Spot)
blackScholesCall :: Double -> Double -> Double -> Double -> Double -> Double
blackScholesCall s k t r sigma
    | t <= 0    = max 0 (s - k)
    | otherwise = s * normCdf d1 - k * exp (-r * t) * normCdf d2
  where
    d1 = (log (s / k) + (r + sigma**2 / 2) * t) / (sigma * sqrt t)
    d2 = d1 - sigma * sqrt t
    normCdf x = cumulative (normalDistr 0 1) x

-- Black-76 (For Futures)
black76Call :: Double -> Double -> Double -> Double -> Double -> Double
black76Call f k t r sigma
    | t <= 0    = max 0 (f - k)
    | otherwise = exp (-r * t) * (f * normCdf d1 - k * normCdf d2)
  where
    d1 = (log (f / k) + (sigma**2 / 2) * t) / (sigma * sqrt t)
    d2 = d1 - sigma * sqrt t
    normCdf x = cumulative (normalDistr 0 1) x

timeToMaturity :: Day -> Day -> Double
timeToMaturity current expire = max 0 (fromIntegral (diffDays expire current) / 365.0)

-- ==========================================
-- 4. Main Execution
-- ==========================================

main :: IO ()
main = do
    putStrLn "--- WTI Option Analysis: BS (Spot) vs B76 (Futures) ---"

    -- 1. Load & Process Spot Data
    spotRaw <- BL.readFile "/Users/amygdala/Downloads/wti_spot.csv"
    let spotMap = case decodeByName spotRaw of
          Left err -> error ("Spot CSV parse error: " ++ err)
          Right (_, v) ->
            V.foldl' (\acc (SpotRecord d p) ->
                        case p of
                          Just price -> M.insert d price acc
                          Nothing    -> acc
                     ) M.empty v

    -- 2. Load & Process Futures Data
    futRaw <- BL.readFile "/Users/amygdala/Downloads/wti_futures.csv"
    let futMap = case decodeByName futRaw of
          Left err -> error ("Futures CSV parse error: " ++ err)
          Right (_, v) ->
            V.foldl' (\acc (FutRecord d p) ->
                        case p of
                          Just price -> M.insert d price acc
                          Nothing    -> acc
                     ) M.empty v

    -- 3. Run Calculations
    let bsResults = M.mapWithKey (\d s ->
            blackScholesCall s strikePrice (timeToMaturity d expiryDate) riskFreeRate volatility) spotMap

    let b76Results = M.mapWithKey (\d f ->
            black76Call f strikePrice (timeToMaturity d expiryDate) riskFreeRate volatility) futMap

    -- 4. Alignment & Comparison
    let comparisons = M.intersectionWith (,) bsResults b76Results
        diffs = M.elems comparisons
        nInt = length diffs
        n = fromIntegral nInt

    if nInt == 0
      then putStrLn "No matching dates found."
      else do
        let mae = (sum $ map (\(bs, b76) -> abs (bs - b76)) diffs) / n

        putStrLn $ "Days Aligned: " ++ show nInt
        putStrLn $ "Mean Absolute Error: " ++ printf "%.4f" mae

        putStrLn "\nLast 5 Days Comparison:"
        putStrLn "Date       | BS (Spot) | B76 (Future) | Diff"
        let last5 = take 5 $ reverse $ M.toList comparisons
        mapM_ (\(d, (bs, b76)) ->
            putStrLn $ show d ++ " | "
                    ++ printf "%8.2f" bs ++ " | "
                    ++ printf "%12.2f" b76 ++ " | "
                    ++ printf "%8.2f" (abs (bs - b76))
              ) last5
