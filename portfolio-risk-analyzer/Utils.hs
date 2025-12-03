module Utils where

splitByComma :: String -> [String]
splitByComma s =
    case dropWhile (== ',') s of
        "" -> []
        s' -> w : splitByComma s''
            where (w, s'') = break (== ',') s'

parseCSV :: String -> [(String, [Double])]
parseCSV content =
    map parseLine (lines content)
  where
    parseLine line =
        let parts = splitByComma line
            name = head parts
            prices = map read (tail parts)
        in (name, prices)

cleanPrices :: [Double] -> [Double]
cleanPrices = filter (> 0)

returns :: [Double] -> [Double]
returns xs = zipWith (\a b -> (b - a) / a) xs (tail xs)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

variance :: [Double] -> Double
variance xs = let m = mean xs in mean [ (x - m)^2 | x <- xs ]

volatility :: [Double] -> Double
volatility = sqrt . variance

corr :: [Double] -> [Double] -> Double
corr xs ys =
    let mx = mean xs
        my = mean ys
        num = sum [ (x - mx)*(y - my) | (x,y) <- zip xs ys ]
        denom = sqrt (sum [ (x - mx)^2 | x <- xs ]) * sqrt (sum [ (y - my)^2 | y <- ys ])
    in num / denom

formatDouble :: Double -> String
formatDouble x = take 6 (show x)
