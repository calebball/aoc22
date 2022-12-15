import Data.Char (isDigit)
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

blocksInRow :: Int -> Sensor -> [Int]
blocksInRow y s = let
    (sensorX, sensorY) = position s
    halfInterval = beaconDistance s - abs (y - sensorY)
  in [sensorX - halfInterval .. sensorX + halfInterval]

solvePart1 :: Int -> [Sensor] -> Int
solvePart1 y sensors = let
    covered = Set.unions . map (Set.fromList . blocksInRow y) $ sensors
    beacons = Set.fromList . map fst . filter (\b -> snd b == y) . map nearestBeacon $ sensors
  in Set.size (Set.difference covered beacons)


main = do
  [rowString, inputFile] <- getArgs
  input <- readFile inputFile
  let row = read rowString
  let sensors = fst . last . P.readP_to_S parseInput $ input
  print . solvePart1 row $ sensors
