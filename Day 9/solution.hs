import Data.List.Split (splitOn)

main :: IO ()
main = do
    fileContents <- readFile "example.txt"
    -- fileContents <- readFile "input.txt"
    let preparedContents = parseInput fileContents
    putStrLn $ solvePart1 preparedContents
    putStrLn $ solvePart2 preparedContents

data Movement = U Int | R Int | D Int | L Int

parseMovement :: [String] -> [Movement]
parseMovement = map ( \y ->
        case splitOn " " y of
            [a, b] -> case a of
                        "U" -> U $ read b
                        "D" -> D $ read b
                        "L" -> L $ read b
                        "R" -> R $ read b
                        _   -> error $ "Unexpected character: " ++ a
            _      -> error $ "Unexpected array: " ++ y
    )

type Point = (Int, Int)
type HeadPosition = Point
type TailPosition = Point

doMovement :: (HeadPosition, TailPosition) -> Movement -> (HeadPosition, TailPosition)
doMovement ((x, y), (x1, y1)) move = _

parseInput :: String -> [Movement]
parseInput = parseMovement . splitOn "\n"

solvePart1 :: [Movement] -> String
solvePart1 x = ""

solvePart2 :: [Movement] -> String
solvePart2 x = ""