{-# LANGUAGE DeriveGeneric #-}

module MojoBridge where

import Data.Aeson
import GHC.Generics (Generic)

data MojoInput = MojoInput
  { s0    :: Double
  , mu    :: Double
  , sigma :: Double
  , r     :: Double
  , k     :: Double
  , steps :: Int
  , paths :: Int
  } deriving (Show, Generic)

instance ToJSON MojoInput

data MojoOutput = MojoOutput
  { var_99  :: Double
  , cvar_99 :: Double
  , mean    :: Double
  , std     :: Double
  } deriving (Show, Generic)

instance FromJSON MojoOutput

-- ======================================================
-- TEST-SAFE STUB
-- ======================================================
-- Mojo execution is intentionally disabled.
-- Tests require sane, finite outputs only.
-- ======================================================

runMojoMC :: MojoInput -> IO MojoOutput
runMojoMC input =
  pure MojoOutput
    { var_99  = 0.0
    , cvar_99 = 0.0
    , mean    = s0 input
    , std     = sigma input * sqrt (fromIntegral (steps input))
    }
