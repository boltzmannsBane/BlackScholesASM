module MojoBridge where

import System.Process
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data MojoInput = MojoInput
  { s0    :: Double
  , mu    :: Double
  , sigma :: Double
  , r     :: Double
  , k     :: Double
  , steps :: Int
  , paths :: Int
  } deriving Show

instance ToJSON MojoInput

data MojoOutput = MojoOutput
  { var_99 :: Double
  , cvar_99 :: Double
  , mean :: Double
  , std :: Double
  } deriving Show

instance FromJSON MojoOutput

runMojoMC :: MojoInput -> IO MojoOutput
runMojoMC input = do
  B.writeFile "input.json" (encode input)
  _ <- callCommand "mojo run mojo/bridge.mojo"
  out <- B.readFile "output.json"
  case decode out of
    Just r  -> return r
    Nothing -> error "Failed to decode Mojo output"
