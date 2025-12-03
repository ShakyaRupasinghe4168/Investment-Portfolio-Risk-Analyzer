module Processing where

import DataTypes
import Utils
import Control.Parallel.Strategies
import Control.DeepSeq

-- Main pipeline: clean → compute stats → correlation matrix
runAnalysisPipeline :: AssetTable -> FinalReport
runAnalysisPipeline table =
    let cleaned = map (\(n,p) -> (n, cleanPrices p)) table
        reports = computeAllAssetStats cleaned
        corr = correlationMatrix (map arReturns reports)
    in FinalReport reports corr

-- Parallel processing of all assets
computeAllAssetStats :: AssetTable -> [AssetReport]
computeAllAssetStats table =
    let work = map computeOne table
    in work `using` parList rdeepseq

-- Compute stats for one asset
computeOne :: (AssetName, PriceSeries) -> AssetReport
computeOne (name, prices) =
    let rets = returns prices
        vol  = volatility rets
        meanR = mean rets
    in AssetReport name rets vol meanR

-- Build correlation matrix across assets
correlationMatrix :: [[Double]] -> [[Double]]
correlationMatrix xs =
    [ [ corr a b | b <- xs ] | a <- xs ]
