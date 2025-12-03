module Main where

import IOHandler (loadData, getUserInputMethod, printReport)
import Processing (runAnalysisPipeline)

main :: IO ()
main = do
    putStrLn "=== Investment Portfolio Risk Analyzer ==="
    method <- getUserInputMethod
    assetData <- loadData method
    let report = runAnalysisPipeline assetData
    printReport report
