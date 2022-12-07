import Data.List.Split (splitOn)
import Data.Set (fromList)

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    putStrLn $ solvePart1 fileContents
    putStrLn $ solvePart2 fileContents

solvePart1 :: String -> String
solvePart1 x = show $ findPacketMarker 4 x

stringIsUnique :: String -> Bool
stringIsUnique x = length x == length (fromList x)

-- Find the character position at which the first occurrence of 'x' unique characters in a row appears
findPacketMarker :: Int -> String -> Int
findPacketMarker x ys | stringIsUnique (take x ys) = x
                      | otherwise                  = findPacketMarker' (x+1) x (tail ys)

findPacketMarker' :: Int -> Int -> String -> Int
findPacketMarker' x y zs | stringIsUnique (take y zs) = x
                         | otherwise                  = findPacketMarker' (x+1) y (tail zs)

solvePart2 :: String -> String
solvePart2 x = show $ findPacketMarker 14 x