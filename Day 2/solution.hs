import Data.List
import Data.Map (Map, fromList)
import Data.List.Split ( splitOn )

-- A, X = Rock
-- B, Y = Paper
-- C, Z = Scissors 

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    putStrLn $ solvePart1 fileContents
    putStrLn $ solvePart2 fileContents

data Outcome = Win | Loss | Draw deriving (Show, Eq, Ord)
data Hand = Rock | Paper | Scissors deriving (Show, Eq, Ord)

solvePart1 :: String -> String
solvePart1 = show . sum . map (
        handArrayToScore . 
        (map (parseHand . head) . words)
    ) . splitOn "\n"

handArrayToScore :: [Hand] -> Int
handArrayToScore (x:y:_) = totalScore y x
handArrayToScore [] = 0
handArrayToScore x = error $ "UnexpectedList: " ++ show x

rps :: Hand -> Hand -> Outcome
rps Rock Rock = Draw
rps Rock Paper = Loss
rps Rock Scissors = Win
rps Paper Rock = Win
rps Paper Paper = Draw
rps Paper Scissors = Loss
rps Scissors Rock = Loss
rps Scissors Paper = Win
rps Scissors Scissors = Draw

parseHand :: Char -> Hand
parseHand x = case x of 'A' -> Rock
                        'B' -> Paper
                        'C' -> Scissors
                        'X' -> Rock
                        'Y' -> Paper
                        'Z' -> Scissors

handScore :: Hand -> Int
handScore x = case x of Rock -> 1
                        Paper -> 2
                        Scissors -> 3

outcomeScore :: Outcome -> Int
outcomeScore x = case x of Win -> 6
                           Loss -> 0
                           Draw -> 3

totalScore :: Hand -> Hand -> Int
totalScore x y = handScore x + outcomeScore (rps x y)

solvePart2 :: String -> String
solvePart2 = show . sum . map (
        handOutcomeToScore . parseHandOutcome . words
    ) . splitOn "\n"

parseOutcome :: Char -> Outcome
parseOutcome x = case x of 'X' -> Loss
                           'Y' -> Draw
                           'Z' -> Win
                           a -> error $ "Found unexpected character '" ++ [x] ++ "' when parsing Outcome" 

parseHandOutcome :: [String] -> Maybe (Hand, Outcome)
parseHandOutcome (x:xs:_) = Just (parseHand $ head x, parseOutcome $ head xs)
parseHandOutcome [] = Nothing
parseHandOutcome ho = error $ "Unexpected array when parsing (h, o): " ++ show ho

outcomeToHand :: Hand -> Outcome -> Hand
outcomeToHand h o = case (h,o) of (x, Draw) -> x
                                  (Rock, Win) -> Paper
                                  (Rock, Loss) -> Scissors
                                  (Scissors, Win) -> Rock
                                  (Scissors, Loss) -> Paper
                                  (Paper, Win) -> Scissors
                                  (Paper, Loss) -> Rock

handOutcomeToScore :: Maybe (Hand, Outcome) -> Int
handOutcomeToScore (Just (h, o)) = handScore (outcomeToHand h o) + outcomeScore o
handOutcomeToScore Nothing = 0