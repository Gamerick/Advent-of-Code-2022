main :: IO ()
main = do
    fileContents <- readFile "example.txt"
    -- fileContents <- readFile "input.txt"
    let preparedContents = parseInput fileContents
    putStrLn $ solvePart1 preparedContents
    putStrLn $ solvePart2 preparedContents

parseInput :: String -> a
parseInput x = _

solvePart1 :: a -> String
solvePart1 x = ""

solvePart2 :: a -> String
solvePart2 x = ""