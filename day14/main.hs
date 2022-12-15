import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Map as Map
import System.Environment
import Text.ParserCombinators.ReadP as P

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


type Grid = Map.Map Int [Int]
  
makeGrid :: [Path] -> Grid
makeGrid ps = Map.fromList . map (\y -> (y, concatMap (intersectsRow y) ps)) $ [0 .. lastRow ps]

intersectsRow :: Int -> Path -> [Int]
intersectsRow y ((x1, y1), (x2, y2))
  | x1 == x2 && y1 <= y && y2 >= y = [x1]
  | x1 == x2 && y2 <= y && y1 >= y = [x1]
  | y1 == y && y2 == y && x1 <= x2 = [x1 .. x2]
  | y1 == y && y2 == y && x2 <= x1 = [x2 .. x1]
  | otherwise = []

inRow :: Int -> Grid -> [Int]
inRow y = fromMaybe [] . Map.lookup y

lastRow :: [Path] -> Int
lastRow = maximum . concatMap (\((x1, y1), (x2, y2)) -> [y1, y2])
  
inVoid :: Int -> Grid -> Bool
inVoid y = (y >) . maximum . Map.keys

inGrid :: Coordinate -> Grid -> Bool
inGrid (x, y) = elem x . inRow y
  
moveSand :: Grid -> Coordinate -> Maybe Coordinate
moveSand g (x, y)
  | inGrid (x, y) g = Nothing
  | inVoid y g = Nothing
  | x `notElem` nextRow = moveSand g (x, y + 1)
  | (x - 1) `notElem` nextRow = moveSand g (x - 1, y + 1)
  | (x + 1) `notElem` nextRow = moveSand g (x + 1, y + 1)
  | otherwise = Just (x, y)
  where nextRow = inRow (y + 1) g
  
addSand :: Maybe Grid -> Maybe Grid
addSand (Just g) = case moveSand g (500, 0) of
  Just (x, y) -> Just (Map.insertWith (++) y [x] g)
  Nothing -> Nothing
addSand Nothing = Nothing

solve :: Grid -> Int
solve = (\n -> n - 1) . length . takeWhile isJust . iterate addSand . Just


main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let paths = concat . fst . last . P.readP_to_S parsePaths $ input
  let y = lastRow paths + 2
  let floor = ((495 - y, y), (505 + y, y))
  putStr "Part 1: "
  print . solve $ makeGrid paths
  putStr "Part 2: "
  print . solve $ makeGrid (floor:paths)
