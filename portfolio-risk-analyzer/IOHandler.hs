module IOHandler where

import DataTypes
import Utils

getUserInputMethod :: IO String
getUserInputMethod = do
    putStrLn "Choose input method:"
    putStrLn "1 - Load CSV file"
    putStrLn "2 - Enter manually"
    getLine

loadData :: String -> IO AssetTable
loadData "1" = do
    putStrLn "Enter CSV file name (example: sample_portfolio.csv):"
    file <- getLine
    content <- readFile file
    return (parseCSV content)
loadData "2" = do
    putStrLn "Enter asset name:"
    name <- getLine
    putStrLn "Enter comma-separated prices:"
    p <- getLine
    let prices = map read (splitByComma p)
    return [(name, prices)]
loadData _ = loadData "2"

printReport :: FinalReport -> IO ()
printReport (FinalReport assets corr) = do
    putStrLn "\n=== Final Report ===\n"
    mapM_ printOne assets
    putStrLn "\nCorrelation Matrix:"
    mapM_ (putStrLn . unwords . map formatDouble) corr

printOne :: AssetReport -> IO ()
printOne a = do
    putStrLn ("Asset: " ++ arName a)
    putStrLn ("  Mean Return   : " ++ formatDouble (arMeanReturn a))
    putStrLn ("  Volatility    : " ++ formatDouble (arVolatility a))
    putStrLn ""
