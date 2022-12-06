import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    putStrLn $ solvePart1 (splitOn "\n" fileContents)
    putStrLn $ solvePart2 (splitOn "\n" fileContents)

solvePart1 :: [String] -> String
solvePart1 = show . sum . map (charPriority . head) . filter (not . null) . map (uncurry intersect . splitStringInHalf)

listCharToPriority :: [(Char, Int)]
listCharToPriority = zip (['a'..'z'] ++ ['A'..'Z']) [1..52]

charPriority :: Char -> Int
charPriority x = case lookup x listCharToPriority of Just y -> y
                                                     Nothing -> error $ "char '" ++ [x] ++ "' not in list"

splitStringInHalf :: String -> (String, String)
splitStringInHalf x = (take n x, take n $ reverse x)
                    where n = length x `div` 2

solvePart2 :: [String] -> String
solvePart2 = show . groupTotals

groupTotals :: [String] -> Int
groupTotals [""] = 0
groupTotals [] = 0
groupTotals (x:y:z:rest) = charPriority (head $ intersect x $ intersect y z) + groupTotals rest
groupTotals l = error $ "Less than 3 groups remain: " ++ show l