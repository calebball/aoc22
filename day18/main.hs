import Data.Char (isDigit)
import Data.List (groupBy, partition, nub, (\\), intercalate)
import Data.Maybe (mapMaybe)
import System.Environment
import Text.ParserCombinators.ReadP as P
import Debug.Trace

type Droplet = (Int, Int, Int)
type Space = (Int, Int, Int)

data Side = Xplus | Xminus | Yplus | Yminus | Zplus | Zminus
  deriving Eq

parseDroplet :: ReadP Droplet
parseDroplet = do
  coords <- map read <$> P.sepBy (P.munch1 isDigit) (P.char ',')
  case coords of
    [x,y,z] -> return (x,y,z)
    _wrongLength -> P.pfail
    
parseInput :: ReadP [Droplet]
parseInput = P.sepBy parseDroplet (P.char '\n')

adjacent :: Droplet -> Droplet -> Bool
adjacent (x1,y1,z1) (x2,y2,z2) = (== 1) . sum . map abs $ [x1 - x2, y1 - y2, z1 - z2]
  
buildCluster :: [Droplet] -> [Droplet] -> ([Droplet], [Droplet])
buildCluster initial = foldl checkAdjacency (initial, [])
  where
    checkAdjacency (inCluster, leftover) d = if any (adjacent d) inCluster
      then let (nowConnected, stillLeftover) = buildCluster [d] leftover in (nowConnected ++ inCluster, stillLeftover)
      else (inCluster, d:leftover)
      
cluster :: [Droplet] -> [[Droplet]]
cluster [] = []
cluster (d:ds) = let (newCluster, remaining) = buildCluster [d] ds in newCluster : cluster remaining

occludes :: Droplet -> Droplet -> Maybe Side
occludes (x1,y1,z1) (x2,y2,z2)
  | x1 == x2 && y1 == y2 && z1 < z2 = Just Zminus
  | x1 == x2 && y1 == y2 && z1 > z2 = Just Zplus
  | x1 == x2 && y1 < y2 && z1 == z2 = Just Yminus
  | x1 == x2 && y1 > y2 && z1 == z2 = Just Yplus
  | x1 < x2 && y1 == y2 && z1 == z2 = Just Xminus
  | x1 > x2 && y1 == y2 && z1 == z2 = Just Xplus
  | otherwise = Nothing

dropletsOcclude :: [Droplet] -> Space -> Bool
dropletsOcclude ds s = (== 6) . length . nub . mapMaybe (occludes s) $ ds 
  
findSpaces :: [Droplet] -> [Space]
findSpaces [] = []
findSpaces ds = let
    minX = minimum . map (\(x, _, _) -> x) $ ds
    maxX = maximum . map (\(x, _, _) -> x) $ ds
    minY = minimum . map (\(_, y, _) -> y) $ ds
    maxY = maximum . map (\(_, y, _) -> y) $ ds
    minZ = minimum . map (\(_, _, z) -> z) $ ds
    maxZ = maximum . map (\(_, _, z) -> z) $ ds
  in [(x,y,z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]] \\ ds

isOpen :: [Droplet] -> [Space] -> Bool
isOpen ds = not . all (dropletsOcclude ds)

solvePart1 :: [Droplet] -> Int
solvePart1 [] = 0
solvePart1 (d:ds) = 6 - 2 * (length . filter (adjacent d) $ ds) + solvePart1 ds

solvePart2 :: [Droplet] -> Int
solvePart2 ds = solvePart1 ds - (sum . map solvePart1 . filter (not . isOpen ds) . cluster . findSpaces $ ds)

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile

  let droplets = fst . last . P.readP_to_S parseInput $ input

  print . solvePart1 $ droplets
  print . solvePart2 $ droplets
