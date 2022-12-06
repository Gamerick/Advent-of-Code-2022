import Data.List
import Data.List.Split ( splitOn )

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    putStrLn $ solvePart1 fileContents
    putStrLn $ solvePart2 fileContents

solvePart1 :: String -> String
solvePart1 = show . maximum . map (sum . map read . words) . splitOn "\n\n"

solvePart2 :: String -> String
solvePart2 = show . sum . take 3 . reverse . sort . map (sum . map read . words) . splitOn "\n\n"