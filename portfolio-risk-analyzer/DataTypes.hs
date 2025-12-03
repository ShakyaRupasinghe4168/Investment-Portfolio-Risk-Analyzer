module DataTypes where

import Control.DeepSeq

type AssetName = String
type PriceSeries = [Double]
type AssetTable = [(AssetName, PriceSeries)]

data AssetReport = AssetReport
    { arName       :: AssetName
    , arReturns    :: [Double]
    , arVolatility :: Double
    , arMeanReturn :: Double
    } deriving (Show)

data FinalReport = FinalReport
    { frAssets     :: [AssetReport]
    , frCorrMatrix :: [[Double]]
    } deriving (Show)

-- Make AssetReport evaluable in parallel
instance NFData AssetReport where
    rnf (AssetReport name rets vol meanR) =
        rnf name `seq` rnf rets `seq` rnf vol `seq` rnf meanR
