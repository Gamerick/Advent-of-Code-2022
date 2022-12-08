import Data.List (find, isPrefixOf, foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace ( trace )

-- Assumptions:
-- - No folder will be entered before it has been listed

main :: IO ()
main = do
    fileContents <- readFile "example.txt"
    -- fileContents <- readFile "input.txt"
    let preparedContents = parseInput fileContents
    putStrLn $ solvePart1 preparedContents
    putStrLn $ solvePart2 preparedContents

type FileSize = Int
type File = (String, FileSize)
data Directory = Directory {
        name :: String,
        subDirectories :: [Directory],
        fileSize :: Int,
        parent :: Maybe Directory
    } deriving (Show)

data DirectoryCommand = FileDesc String Int | DirDesc String deriving (Show)
data DollarCommand = ChangeDirectory String | ListDirectory [DirectoryCommand] deriving (Show)

parseInput :: String -> [DollarCommand]
parseInput = map parseDollarCommand . filter (not . null) . splitOn "$ "

parseDirectoryCommands :: String -> [DirectoryCommand]
parseDirectoryCommands x = map (
        \y -> case splitOn " " y of ("dir":rest:_) -> DirDesc rest
                                    (a:as:_)       -> FileDesc as (read a)
                                    z              -> error $ "Found unexpected input: " ++ show z
    ) $ filter (not . null) (splitOn "\n" x)

parseDollarCommand :: String -> DollarCommand
parseDollarCommand x | "cd" `isPrefixOf` x = ChangeDirectory $ init $ rest ' '
                     | "ls" `isPrefixOf` x = ListDirectory $ parseDirectoryCommands $ rest '\n'
                     | otherwise           = error $ "Unexpected command string: " ++ x
                     where rest z = tail $ dropWhile (/= z) x

directorySize :: Directory -> FileSize
directorySize d = fileSize d + sum (map directorySize $ subDirectories d)

allDirectorySizes :: Directory -> [(Directory, FileSize)]
allDirectorySizes d | null $ subDirectories d = [(d, directorySize d)]
                    | otherwise               = (d, directorySize d) : concatMap allDirectorySizes (subDirectories d)

findSubDirectory :: Directory -> String -> Directory
findSubDirectory d s = case find (\subD -> s == name subD) (subDirectories d) of Just x  -> x
                                                                                 Nothing -> error $ "SubDirectory '" ++ s ++ "' doesn't exist in directory " ++ show d

rootDirectory :: Directory
rootDirectory = Directory {name="\\", subDirectories=[], fileSize=0, parent=Nothing}

doCommand :: Directory -> DollarCommand -> Directory
-- doCommand a b | trace ("doCommand, dir: " ++ show a ++ ", cmd: " ++ show b) False = undefined
doCommand dir (ChangeDirectory "..") = case parent dir of Just x  -> x
                                                          Nothing -> error $ "Directory " ++ name dir ++ " doesn't have a parent"
doCommand dir (ChangeDirectory x)  = findSubDirectory dir x
doCommand dir (ListDirectory cmds) = foldl doListCommand dir cmds

doListCommand :: Directory -> DirectoryCommand -> Directory
doListCommand a b | trace ("doListCommand, dir: " ++ show a ++ "\ncmd: " ++ show b ++ "\n") False = undefined
doListCommand Directory {
        name=n, subDirectories=sd, fileSize=fs, parent=p
    } (FileDesc s i) = Directory {
        name=n, subDirectories=sd, fileSize=fs + i, parent=p
    }
doListCommand dir (DirDesc s) = Directory{
        name=name dir, subDirectories=subDirectories dir ++ [Directory {
            name=s, subDirectories=[], fileSize=0, parent=Just dir
        }] , fileSize=fileSize dir, parent=parent dir
    }

solvePart1 :: [DollarCommand] -> String
solvePart1 = show . foldl' doCommand rootDirectory

solvePart2 :: [DollarCommand] -> String
solvePart2 x = ""