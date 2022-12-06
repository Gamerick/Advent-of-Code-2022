import Data.List.Split (splitOn)

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    putStrLn $ solvePart1 (splitOn "\n" fileContents)
    putStrLn $ solvePart2 (splitOn "\n" fileContents)

solvePart1 :: [String] -> String
solvePart1 = show . occurences True
 . map (uncurry rangeFullyContainedWithin)
 . parse . filter (not . null)

parse :: [String] -> [((Int, Int), (Int, Int))]
parse = map ( \y ->
        case splitOn "," y of
            [a, b] -> (rangeToTuple a, rangeToTuple b)
            _ -> error $ "Unexpected array: " ++ show y
    )

-- Expects a string of the format "36-78"
rangeToTuple :: String -> (Int, Int)
rangeToTuple x = case splitOn "-" x of
                        [a, b] -> (read a, read b)
                        z -> error $ "Unexpected array: " ++ show z

rangeFullyContainedWithin :: (Int, Int) -> (Int, Int) -> Bool
rangeFullyContainedWithin (a, b) (x, y) | b - a < y - x  = (a >= x) && (b <= y)
                                        | b - a >= y - x = (a <= x) && (b >= y)

occurences :: Eq a => a -> [a] -> Int
occurences x = length . filter (==x)

solvePart2 :: [String] -> String
solvePart2 = show . occurences True
 . map (uncurry rangePartiallyContainedWithin)
 . parse . filter (not . null)

rangePartiallyContainedWithin :: (Int, Int) -> (Int, Int) -> Bool
rangePartiallyContainedWithin (a, b) (x, y) | b - a < y - x = between a (x, y) || between b (x, y)
                                            | otherwise     = between x (a, b) || between y (a, b)

between :: Ord a => a -> (a, a) -> Bool
between x (y, z) = x >= y && x <= z