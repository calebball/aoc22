import Data.Char (ord)
import Data.List (transpose, find, groupBy, elemIndex)
import Data.Graph
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.Environment

type Height = Int
type HeightMap = [[Int]]
type Position = (Int, Int)

parseHeight :: Char -> Height
parseHeight 'S' = 1
parseHeight 'E' = 26
parseHeight c = ord c - ord 'a' + 1

parseHeightMap :: String -> HeightMap
parseHeightMap = map (map parseHeight) . lines

lookUpHeight :: HeightMap -> Position -> Height
lookUpHeight m (x, y) = (m !! y) !! x

lastColumn :: HeightMap -> Int
lastColumn = length . head

lastRow :: HeightMap -> Int
lastRow = length

orthogonalPositions :: HeightMap -> Position -> [Position]
orthogonalPositions m (x, y) = let
    lastX = lastColumn m
    lastY = lastRow m
    checkBounds (x', y') = x' >= 0 && x' < lastX && y' >= 0 && y' < lastY
  in filter checkBounds [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    
reachablePositions :: HeightMap -> Position -> [Position]
reachablePositions m p = let
    currentHeight = lookUpHeight m p
    checkHeight p' = lookUpHeight m p' <= currentHeight + 1
  in filter checkHeight (orthogonalPositions m p)
  
  
positionToKey :: HeightMap -> Position -> Int
positionToKey m (x, y) = lastColumn m * y + x

keyToPosition :: HeightMap -> Int -> Position
keyToPosition m i = (i `rem` lastColumn m, i `div` lastColumn m)

positions :: HeightMap -> [Position]
positions m = map (keyToPosition m) [0 .. lastRow m * lastColumn m - 1]


buildGraph :: HeightMap -> (Graph, Vertex -> (Int, Int, [Int]), Int -> Maybe Vertex)
buildGraph m = graphFromEdges (map (buildNodeTuple m) (positions m))
  where buildNodeTuple m p = (lookUpHeight m p, positionToKey m p, map (positionToKey m) (reachablePositions m p))
  
searchStep :: HeightMap -> (Graph, Vertex -> (Int, Int, [Int]), Int -> Maybe Vertex) -> Map.Map Position Int -> Map.Map Position Int
searchStep m (_, vertexToNode, keyToVertex) state = let
    thd (_, _, c) = c
    currentIter = maximum (Map.elems state)
    currentKeys = map (positionToKey m) . Map.keys . Map.filter (== currentIter) $ state
    nextKeys = concatMap (thd . vertexToNode . fromJust . keyToVertex) currentKeys
    nextPositions = map (keyToPosition m) nextKeys
  in Map.union state (Map.fromList . zip nextPositions . repeat $ currentIter + 1)


startPosition :: HeightMap -> String -> Position
startPosition m = keyToPosition m . fromJust . elemIndex 'S' . filter (/= '\n')

endPosition :: HeightMap -> String -> Position
endPosition m = keyToPosition m . fromJust . elemIndex 'E' . filter (/= '\n')


formatInt :: Int -> String
formatInt i
  | i == 0 = " S"
  | i == 27 = " E"
  | i < 10 = " " ++ show i
  | otherwise = show i

formatD :: Maybe Int -> String
formatD Nothing = "  ."
formatD (Just i)
  | i == 0 = "  S"
  | i < 10 = "  " ++ show i
  | i < 100 = " " ++ show i
  | otherwise = show i


main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let heights = parseHeightMap input
  putStrLn . unlines . map (unwords . map formatInt) . transpose $ heights
  let graph = buildGraph heights
  let start = startPosition heights input
  let end = endPosition heights input
  print start
  print end
  let search = iterate (searchStep heights graph) (Map.fromList [(start, 0)])
  let completeSearch = fromJust . find (Map.member end) $ search
  putStrLn . unlines
           . map (unwords . map formatD)
           . transpose
           . map (map (\p -> Map.lookup p $ completeSearch
      ))
           . groupBy (\a b -> snd a == snd b)
           . positions
           $ heights
  print . Map.lookup end $ completeSearch
