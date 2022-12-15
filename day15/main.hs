import Data.Char (isDigit)
import Data.List (findIndex, nub, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import System.Environment
import Text.ParserCombinators.ReadP as P

type Coordinate = (Int, Int)

parseCoordinate :: ReadP Coordinate
parseCoordinate = do
  P.string "x="
  x <- read <$> P.munch1 (\c -> c == '-' || isDigit c)
  P.string ", y="
  y <- read <$> P.munch1 (\c -> c == '-' || isDigit c)
  return (x, y)
  
type Beacon = Coordinate
data Sensor = Sensor { position :: Coordinate, nearestBeacon :: Beacon }
  deriving Show

parseSensor :: ReadP Sensor
parseSensor = do
  P.string "Sensor at "
  position <- parseCoordinate
  P.string ": closest beacon is at "
  nearestBeacon <- parseCoordinate
  return (Sensor position nearestBeacon)

parseInput :: ReadP [Sensor]
parseInput = P.sepBy parseSensor (P.char '\n')


yDistance :: Coordinate -> Coordinate -> Int
yDistance (_, y1) (_, y2) = abs (y2 - y1)

xDistance :: Coordinate -> Coordinate -> Int
xDistance (x1, _) (x2, _) = abs (x2 - x1)

distance :: Coordinate -> Coordinate -> Int
distance c1 c2 = xDistance c1 c2 + yDistance c1 c2

beaconDistance :: Sensor -> Int
beaconDistance (Sensor s b) = distance s b

data Interval = Interval { start :: Int, end :: Int }

instance Show Interval where
  show (Interval s e) = "[" ++ show s ++ ", " ++ show e ++ "]"

sizeOfInterval :: Interval -> Int
sizeOfInterval (Interval s e) = e - s + 1

areDisjoint :: Interval -> Interval -> Bool
areDisjoint (Interval startA endA) (Interval startB endB)
  | startB > endA = True
  | startA > endB = True
  | otherwise = False

unionIntervals :: [Interval] -> [Interval]
unionIntervals (a : b : xs)
  | areDisjoint a b = a : unionIntervals (b:xs)
  | end a >= start b = unionIntervals (Interval (minimum . map start $ [a, b]) (maximum . map end $ [a, b]):xs)
  | end b >= start a = unionIntervals (Interval (minimum . map start $ [a, b]) (maximum . map end $ [a, b]):xs)
  | otherwise = a : unionIntervals (b:xs)
unionIntervals xs = xs

subtractInterval :: Interval -> Interval -> [Interval]
subtractInterval (Interval startA endA) (Interval startB endB)
  | startA == startB && endA == endB = []
  | startA == startB && endA > endB = [Interval endB endA]
  | startA == startB && endA < endB = []
  | startA < startB && endA == endB = [Interval startA startB]
  | startA < startB && endA > endB = [Interval startA startB, Interval endA endB]
  | startA < startB && endA < endB = [Interval startA startB]
  | startA > startB && endA == endB = []
  | startA > startB && endA > endB = [Interval endB endA]
  | startA > startB && endA < endB = []
  | otherwise = []
  
differenceIntervals :: [Interval] -> [Interval] -> [Interval]
differenceIntervals [] _ = []
differenceIntervals (a:as) bs = concatMap (subtractInterval a) bs ++ differenceIntervals as bs


blocksInRow :: Int -> Sensor -> Maybe Interval
blocksInRow y s = let
    (sensorX, sensorY) = position s
    halfInterval = beaconDistance s - abs (y - sensorY)
  in if halfInterval < 0
    then Nothing
    else Just (Interval (sensorX - halfInterval) (sensorX + halfInterval))

solvePart1 :: Int -> [Sensor] -> Int
solvePart1 y sensors = let
    covered = unionIntervals . mapMaybe (blocksInRow y) $ sensors
    numCovered = sum . map sizeOfInterval $ covered
    numBeacons = length . nub . filter (\b -> snd b == y) . map nearestBeacon $ sensors
  in numCovered - numBeacons
  
solvePart2 :: Int -> [Int] -> [Sensor] -> Maybe Int
solvePart2 bound (y:ys) sensors = let
    allIntervals = sortBy (\a b -> compare (start a) (start b)) . mapMaybe (blocksInRow y) $ sensors
    covered = unionIntervals allIntervals
    uncovered = differenceIntervals [Interval 0 bound] covered
  in if null uncovered then solvePart2 bound ys sensors else
    let
      x = start (head uncovered) + 1
    in Just (x * 4000000 + y)
solvePart2 _ [] _ = Nothing


main = do
  [inputValue, inputFile] <- getArgs
  input <- readFile inputFile
  let value = read inputValue
  let sensors = fst . last . P.readP_to_S parseInput $ input
  print . solvePart2 value [0..value] $ sensors
