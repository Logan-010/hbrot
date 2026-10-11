module Cli (Cli (..), parse) where

import Colormaps (Colormap)

data Cli = Cli
  { dimensions :: (Int, Int),
    imageCenter :: Maybe (Double, Double),
    viewSize :: Maybe (Double, Double),
    stepLimits :: (Int, Int),
    colormap :: Maybe Colormap,
    rngSeed :: Maybe Int,
    threads :: Maybe Int,
    bailoutNum :: Maybe Double,
    fileName :: Maybe String,
    noInfo :: Bool
  }
  deriving (Show, Eq)

parse :: [String] -> Either String Cli
parse args = Right (Cli {})
