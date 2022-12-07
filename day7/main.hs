import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isPrint)
import Data.List (sort, replicate)
import Data.Maybe (mapMaybe)
import System.Environment
import Text.ParserCombinators.ReadP as P
import GHC.IO


data LsResult = LsDirectory String | LsFile String Int
  deriving Show

data Command = Cd String | Ls [LsResult]
  deriving Show


parseLsDirectory :: P.ReadP LsResult
parseLsDirectory = do
  P.string "dir"
  P.skipSpaces
  dirName <- P.munch1 isPrint
  return (LsDirectory dirName)
  
parseLsFile :: P.ReadP LsResult
parseLsFile = do
  size <- P.munch1 isDigit
  P.skipSpaces
  fileName <- P.munch1 isPrint
  return (LsFile fileName (read size))
  
parseLsResult :: P.ReadP LsResult
parseLsResult = parseLsDirectory <|> parseLsFile

parseCd :: P.ReadP Command
parseCd = do
  P.string "cd"
  P.skipSpaces
  dirName <- P.munch1 isPrint
  return (Cd dirName)

parseLs :: P.ReadP Command
parseLs = do
  P.string "ls"
  P.char '\n'
  results <- P.sepBy parseLsResult (P.char '\n')
  return (Ls results)

parseCommand :: P.ReadP Command
parseCommand = do
  P.char '$'
  P.skipSpaces
  parseCd <|> parseLs
  
parseInput :: P.ReadP [Command]
parseInput = P.sepBy parseCommand (P.char '\n')

isAscend :: Command -> Bool
isAscend (Cd "..") = True
isAscend _ = False

isDescend :: Command -> Bool
isDescend (Cd n) = n /= ".."
isDescend _ = False

matchAscends :: [Command] -> [Command]
matchAscends cs = cs ++ replicate (count isDescend cs - count isAscend cs - 1) (Cd "..")
  where count p ls = length . filter p $ ls


data FsEntry = Directory String [FsEntry] | File String Int
  deriving Show
  
applyCommand :: [FsEntry] -> Command -> [FsEntry]
applyCommand (d : Directory p fs : ds) (Cd "..") = Directory p (d:fs) : ds
applyCommand fs (Cd p) = Directory p [] : fs
applyCommand (Directory p fs : ds) (Ls results) =
  Directory p (mapMaybe toFsFile results) : ds
  where toFsFile r = case r of
          LsDirectory _ -> Nothing
          LsFile name size -> Just (File name size)
applyCommand a _ = a

size :: FsEntry -> Int
size (File _ s) = s
size (Directory _ fs) = sum . map size $ fs


directorySizes :: FsEntry -> [(String, Int)]
directorySizes (Directory name (a:fs)) =
  (name, size (Directory name (a:fs))) : concatMap directorySizes (mapMaybe isDir (a:fs))
  where isDir f = case f of
          Directory n a -> Just (Directory n a)
          File _ _ -> Nothing
directorySizes _ = []


solvePart1 :: FsEntry -> Int
solvePart1 = sum . filter (< 100000) . map snd . directorySizes

solvePart2 :: FsEntry -> Int
solvePart2 d = let
    totalSpace = 70000000
    emptySpace = totalSpace - size d
    reqdSpace = 30000000
    needToFree = reqdSpace - emptySpace
  in
    head . dropWhile (< needToFree) . sort . map snd . directorySizes $ d


main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let commands = matchAscends . fst . last . P.readP_to_S parseInput $ input
  let fileSystem = head . foldl applyCommand [] $ commands
  let commandSteps = foldl applyCommand [] commands
  print . solvePart1 $ fileSystem
  print . solvePart2 $ fileSystem
