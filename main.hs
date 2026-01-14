{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative (empty, (<|>))
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Time (Day, defaultTimeLocale, diffDays, parseTimeM, fromGregorian)
import qualified Data.Vector as V
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)

-- ==========================================
-- 1. Configuration
-- ==========================================

strikePrice :: Double
strikePrice = 75.00

riskFreeRate :: Double
riskFreeRate = 0.04 

volatility :: Double
volatility = 0.35 

expiryDate :: Day
expiryDate = fromGregorian 2026 12 31

-- ==========================================
-- 2. Parsing Logic (Robust)
-- ==========================================

-- Type A: Spot CSV (YYYY-MM-DD)
-- We use 'Maybe Double' to safely handle empty cells like "2021-01-18,"
data SpotRecord = SpotRecord 
    { spotDate  :: Day
    , spotPrice :: Maybe Double 
    }

instance FromNamedRecord SpotRecord where
    parseNamedRecord r = SpotRecord
        <$> (parseDate =<< r .: "Date")
        <*> (Just <$> r .: "Price" <|> pure Nothing)
      where
        parseDate s = case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
            Just d -> return d
            Nothing -> fail $ "Spot CSV Date Parse Error: " ++ s


-- Type B: Actual CSV (MM/DD/YYYY)
data ActualRecord = ActualRecord 
    { actDate  :: Day
    , actPrice :: Maybe Double 
    }

instance FromNamedRecord ActualRecord where
    parseNamedRecord r = ActualRecord
        <$> (parseDate =<< r .: "Date")
        <*> (Just <$> r .: "Price" <|> pure Nothing)
      where
        parseDate s = case parseTimeM True defaultTimeLocale "%m/%d/%Y" s of
            Just d -> return d
            Nothing -> fail $ "Actual CSV Date Parse Error: " ++ s

-- Output Type
data ResultRecord = ResultRecord { resDate :: Day, resPrice :: Double }
instance ToNamedRecord ResultRecord where
    toNamedRecord (ResultRecord d p) = namedRecord ["Date" .= show d, "Price" .= p]
instance DefaultOrdered ResultRecord where
    headerOrder _ = header ["Date", "Price"]

-- ==========================================
-- 3. Black-Scholes Logic
-- ==========================================

blackScholesCall :: Double -> Double -> Double -> Double -> Double -> Double
blackScholesCall s k t r sigma
    | t <= 0    = max 0 (s - k)
    | otherwise = s * normCdf d1 - k * exp (-r * t) * normCdf d2
  where
    d1 = (log (s / k) + (r + sigma^2 / 2) * t) / (sigma * sqrt t)
    d2 = d1 - sigma * sqrt t
    normCdf x = cumulative (normalDistr 0 1) x

timeToMaturity :: Day -> Day -> Double
timeToMaturity current expire = max 0 (fromIntegral (diffDays expire current) / 365.0)

-- ==========================================
-- 4. Main Execution
-- ==========================================

-- Helper to load CSV into a Map, filtering out "Nothing" prices
loadCsv :: FromNamedRecord a => (a -> Day) -> (a -> Maybe Double) -> FilePath -> IO (Map Day Double)
loadCsv getDate getPrice fpath = do
    csvData <- BL.readFile fpath
    case decodeByName csvData of
        Left err -> error $ "Failed to read " ++ fpath ++ ": " ++ err
        Right (_, v) -> return $ V.foldl' insertValid M.empty v
  where
    insertValid acc record = 
        case getPrice record of
            Just price -> M.insert (getDate record) price acc
            Nothing    -> acc -- Skip rows where price is empty/invalid

main :: IO ()
main = do
    putStrLn "--- Black-Scholes Accuracy Checker ---"

    -- 1. Load Spot Data
    putStrLn "Loading Spot prices..."
    spotMap <- loadCsv spotDate spotPrice "/Users/amygdala/Downloads/wti_spot.csv"
    putStrLn $ "Loaded " ++ show (M.size spotMap) ++ " valid spot records (skipping empty rows)."

    -- 2. Calculate Black-Scholes
    let calcMap = M.mapWithKey (\d spot -> 
            let t = timeToMaturity d expiryDate
            in blackScholesCall spot strikePrice t riskFreeRate volatility
            ) spotMap

    -- 3. Save Artifact
    putStrLn "Saving calculated BS prices to 'bs_results.csv'..."
    let results = [ResultRecord d p | (d, p) <- M.toList calcMap]
    BL.writeFile "bs_results.csv" $ encodeDefaultOrderedByName results

    -- 4. Load Actual Data
    putStrLn "Loading Actual prices..."
    actualMap <- loadCsv actDate actPrice "/Users/amygdala/Downloads/wti_futures.csv"
    putStrLn $ "Loaded " ++ show (M.size actualMap) ++ " valid actual records."

    -- 5. Compare
    let comparisons = M.intersectionWith (\calc act -> (calc, act)) calcMap actualMap
        diffs       = M.elems comparisons
        n           = fromIntegral (length diffs) :: Double

    if n == 0 
        then putStrLn "Error: No matching dates found between files."
        else do
            let absErrors = map (\(c, a) -> abs (c - a)) diffs
            let sqErrors  = map (\(c, a) -> (c - a) ^ 2) diffs
            let mae       = sum absErrors / n
            let rmse      = sqrt (sum sqErrors / n)

            putStrLn "\n--- Accuracy Report ---"
            putStrLn $ "Matching Days Found:    " ++ show (round n)
            putStrLn $ "Mean Absolute Error:    " ++ show mae
            putStrLn $ "Root Mean Sq Error:     " ++ show rmse

            putStrLn "\nSample Comparisons (Last 3):"
            mapM_ (\(d, (c, a)) -> putStrLn $ show d ++ " | Predicted: " ++ show c ++ " | Actual: " ++ show a) 
                  (take 3 $ reverse $ M.toList comparisons)
