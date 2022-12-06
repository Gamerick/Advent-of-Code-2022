import Data.List.Split (splitOn)

--                         [Z] [W] [Z]
--         [D] [M]         [L] [P] [G]
--     [S] [N] [R]         [S] [F] [N]
--     [N] [J] [W]     [J] [F] [D] [F]
-- [N] [H] [G] [J]     [H] [Q] [H] [P]
-- [V] [J] [T] [F] [H] [Z] [R] [L] [M]
-- [C] [M] [C] [D] [F] [T] [P] [S] [S]
-- [S] [Z] [M] [T] [P] [C] [D] [C] [D]
--  1   2   3   4   5   6   7   8   9 

crates :: [String]
crates = [
        "SCVN",
        "ZMJHNS",
        "MCTGJND",
        "TDFJWRM",
        "PFH",
        "CTZHJ",
        "DPRQFSLZ",
        "CSLHDFPW",
        "DSMPFNGZ"
    ]


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let preparedContents = filter (not . null) $ splitOn "\n" fileContents
    putStrLn $ solvePart1 preparedContents
    putStrLn $ solvePart2 preparedContents

type Instruction = (Int, Int, Int)

parseInstruction :: String -> Instruction
parseInstruction x = case splitOn " " x of 
                        [move,a,from,b,to,c] -> (read a, read b - 1, read c - 1)
                        _                    -> error $ "Unexpected string while parsing: " ++ x

doCrateMove :: [String] -> Instruction -> [String]
doCrateMove crates (n, col1, col2) = insertIntoArray x col1 (insertIntoArray y col2 crates) where
                                        (x, y) = moveStringSuffix (crates!!col1, crates!!col2) n

doPart2CrateMove :: [String] -> Instruction -> [String]
doPart2CrateMove crates (n, col1, col2) = insertIntoArray x col1 (insertIntoArray y col2 crates) where
                                        (x, y) = moveSringSuffixPart2 (crates!!col1, crates!!col2) n


moveStringSuffix :: (String, String) -> Int -> (String, String)
moveStringSuffix (src, dest) x = (
        take (length src - x) src,
        dest ++ take x (reverse src)
    )

moveSringSuffixPart2 :: (String, String) -> Int -> (String, String)
moveSringSuffixPart2 (src, dest) x = (
        take (length src - x) src,
        dest ++ reverse (take x (reverse src))
    )

insertIntoArray :: a -> Int -> [a] -> [a]
insertIntoArray x n xs = l ++ [x] ++ r where
                        l = take n xs
                        r = drop (n + 1) xs

solvePart1 :: [String] -> String
solvePart1 = map last . foldl doCrateMove crates . map parseInstruction


solvePart2 :: [String] -> String
solvePart2 = map last . foldl doPart2CrateMove crates . map parseInstruction