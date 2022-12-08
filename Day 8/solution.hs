import Data.List.Split (splitOn)
import Data.List (transpose)
import GHC.Utils.Misc ( count )

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let preparedContents = parseInput fileContents
    putStrLn $ solvePart1 preparedContents
    putStrLn $ solvePart2 preparedContents

type TreeGrid = [[Int]]

parseInput :: String -> TreeGrid
parseInput = map (map (\x -> read [x])) . filter (not . null) . splitOn "\n"

treeVisible :: Int -> [Int] -> Bool
treeVisible x trees | x > length trees = error $ show x ++ " is greater than the length of the array: " ++ show (length trees)
                    | x == 1           = True
                    | otherwise        = maximum (take (x-1) trees) < trees!!(x - 1)

listVisibility :: [Int] -> [Bool]
listVisibility = listVisibility' 1

listVisibility' :: Int -> [Int] -> [Bool]
listVisibility' x ys | x > length ys = []
listVisibility' x ys = treeVisible x ys : listVisibility' (x+1) ys

arrayOr :: [Bool] -> [Bool] -> [Bool]
arrayOr [] _ = []
arrayOr _ [] = []
arrayOr (x:xs) (y:ys) = (x||y) : arrayOr xs ys

gridOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
gridOr [] _ = []
gridOr _ [] = []
gridOr (xs:xss) (ys:yss) = arrayOr xs ys : gridOr xss yss

solvePart1 :: TreeGrid -> String
solvePart1 grid = show $ count (== True) $ concat
    (map listVisibility grid `gridOr`
    map (reverse.listVisibility.reverse) grid `gridOr`
    transpose (map listVisibility (transpose grid)) `gridOr`
    transpose (map (reverse . listVisibility . reverse) (transpose grid)))

scenicScore :: TreeGrid -> (Int, Int) -> Int
scenicScore trees (x, y) = 
  foldr
  ((*) . visibleTreeCount (trees !! x !! y)) 1
  (visibleTreeArrays trees (x, y))

visibleTreeArrays :: TreeGrid -> (Int, Int) -> [[Int]]
visibleTreeArrays treeGrid (x, y) = [reverse $ fst rowBits, tail $ snd rowBits, reverse $ fst colBits, tail $ snd colBits] where
                            rowBits = splitAt y (treeGrid!!x)
                            colBits = splitAt x (transpose treeGrid !! y)


visibleTreeCount :: Int -> [Int] -> Int
visibleTreeCount height trees = min (length trees) (length (takeWhile (< height) trees) + 1)

treesToCheck :: [(Int, Int)]
treesToCheck = [(a, b) | a <- [1..97], b <- [1..97]]

solvePart2 :: TreeGrid -> String
solvePart2 x = show $ maximum $ map (scenicScore x) treesToCheck