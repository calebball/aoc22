import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)
import System.Environment
import Text.ParserCombinators.ReadP as P
import Debug.Trace

type Coordinate = (Int, Int)

type Path = (Coordinate, Coordinate)

parseCoordinate :: ReadP Coordinate
parseCoordinate = do
  x <- P.munch1 isDigit
  P.char ','
  y <- P.munch1 isDigit
  return (read x, read y)
  
parsePath :: ReadP [Path]
parsePath = do
  cs <- P.sepBy parseCoordinate (P.string " -> ")
  return (buildPairs cs)
  where
    buildPairs :: [Coordinate] -> [Path]
    buildPairs (a:b:cs) = (a, b) : buildPairs (b:cs)
    buildPairs _ = []

parsePaths :: ReadP [[Path]]
parsePaths = P.sepBy parsePath (P.char '\n')


data Space = Empty | Rock | Sand

data Grid = Grid { paths :: [Path], sand :: [Coordinate] }
  deriving Show

intersectsRow :: Int -> Path -> [Int]
intersectsRow y ((x1, y1), (x2, y2))
  | x1 == x2 && y1 <= y && y2 >= y = [x1]
  | x1 == x2 && y2 <= y && y1 >= y = [x1]
  | y1 == y && y2 == y && x1 <= x2 = [x1 .. x2]
  | y1 == y && y2 == y && x2 <= x1 = [x2 .. x1]
  | otherwise = []

inRow :: Grid -> Int -> [Int]
inRow g y = concatMap (intersectsRow y) (paths g) ++ (map fst . filter ((== y) . snd) $ sand g)
  
inVoid :: Grid -> Int -> Bool
inVoid g y = all (y >) . concatMap (\((x1, y1), (x2, y2)) -> [y1, y2]) $ paths g
  
moveSand :: Grid -> Coordinate -> Maybe Coordinate
moveSand g (x, y)
  | inVoid g y = Nothing
  | x `notElem` nextRow = moveSand g (x, y + 1)
  | (x - 1) `notElem` nextRow = moveSand g (x - 1, y + 1)
  | (x + 1) `notElem` nextRow = moveSand g (x + 1, y + 1)
  | otherwise = Just (x, y)
  where nextRow = inRow g (y + 1)
  
moveSand' g c = let r = moveSand g c in trace ("    moveSand from " ++ show c ++ " to " ++ show r) r
  
addSand :: Maybe Grid -> Maybe Grid
addSand (Just g) = case moveSand g (500, 0) of
  Just c -> Just (Grid (paths g) (c : sand g))
  Nothing -> Nothing
addSand Nothing = Nothing

solvePart1 :: Grid -> Int
solvePart1 g = length . sand . last . map fromJust . takeWhile isJust . iterate addSand $ Just g


main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let paths = concat . fst . last . P.readP_to_S parsePaths $ input
  let grid = Grid paths []
  print . solvePart1 $ grid
