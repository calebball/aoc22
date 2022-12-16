import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment
import Text.ParserCombinators.ReadP as P
import Debug.Trace

type ValveKey = String

data Valve = Valve { key :: ValveKey, rate :: Int, tunnels :: Set.Set ValveKey }
  deriving Show
  
type ValveMap = Map.Map ValveKey Valve

parseValve :: ReadP Valve
parseValve = do
  P.string "Valve "
  key <- parseKey
  P.string " has flow rate="
  rate <- read <$> P.munch1 isDigit
  P.string "; tunnels lead to valves " <|> P.string "; tunnel leads to valve "
  tunnels <- Set.fromList <$> P.sepBy parseKey (P.string ", ")
  return (Valve key rate tunnels)
  where parseKey = P.count 2 P.get
  
parseValveMap :: ReadP ValveMap
parseValveMap = do
  valves <- P.sepBy parseValve (P.char '\n')
  return . Map.fromList . map (\v -> (key v, v)) $ valves


data SearchPath = SearchPath { position :: Valve, open :: Set.Set ValveKey, timeRemaining :: Int, currentRelease :: Int, maxHeuristic :: Int, minHeuristic :: Int }
  deriving Show

maxReleaseHeuristic :: ValveMap -> Set.Set ValveKey -> Int -> Int
maxReleaseHeuristic m open timeRemaining = let
    closedValves = Map.withoutKeys m open
  in timeRemaining * (sum . take (timeRemaining `div` 2) . sort . map rate . Map.elems $ closedValves)
  -- in trace ("h w/ " ++ show (Set.toList open)) (timeRemaining * (sum . take (timeRemaining `div` 2) . sort . map rate . Map.elems $ closedValves))
  
branch :: ValveMap -> [SearchPath] -> [[SearchPath]]
branch m (p:ps) = let
    v = position p
    nextTime = timeRemaining p - 1
    nextRelease = currentRelease p + (sum . map rate . Map.elems $ Map.restrictKeys m (open p))
    minHeuristic o = currentRelease p + timeRemaining p * (sum . map rate . Map.elems $ Map.restrictKeys m o)
    makeBranch o v = SearchPath v o nextTime nextRelease (minHeuristic o + maxReleaseHeuristic m o nextTime) (minHeuristic o) : p : ps
    moves = map (makeBranch (open p)) . Map.elems . Map.restrictKeys m $ tunnels v
    nextOpen = Set.insert (key v) (open p)
  in if Set.member (key v) (open p)
    then moves
    else makeBranch nextOpen v : moves
branch _ [] = error "branch failed"

branch' m (p:ps) = let r = branch m (p:ps) in trace ("Branching " ++ showSearchStep p ++ " into\n" ++ (intercalate "\n" . map (showSearchStep . head) $ r)) r
branch' _ [] = error "lolwat"
    
bound :: [[SearchPath]] -> [[SearchPath]]
bound [] = []
bound ps = let
    best = head . maximumBy releaseComparison $ ps
  -- in sortBy heuristicComparison . filter ((>= best) . maxHeuristic . head) $ ps
    (toKeep, toDrop) = partition (\(p:_) -> (minHeuristic p >= minHeuristic best) || (maxHeuristic p >= maxHeuristic best)) ps
  -- in trace ("Best is " ++ show best ++ "\nDropping\n- " ++ (intercalate "\n- " . map (show . head) $  toDrop)) (sortBy heuristicComparison toKeep)
  in sortBy heuristicComparison toKeep
  where
    heuristicComparison (a:_) (b:_) = maxHeuristic b `compare` maxHeuristic a
    heuristicComparison _ _ = error "release failed"
    releaseComparison (a:_) (b:_) = currentRelease b `compare` currentRelease a
    releaseComparison _ _ = error "release failed"
    
bound' ps = let r = bound ps in trace ("Bounding " ++ show (length ps) ++ " searches to\n--  " ++ show (map (maxHeuristic . head) r)) r
    
branchAndBound :: ValveMap -> [[SearchPath]] -> [SearchPath]
branchAndBound m (p:ps)
  | timeRemaining (head p) == 0 = p
  | otherwise = branchAndBound m . bound $ branch m p ++ ps
  -- | otherwise = branchAndBound m . bound' $ branch' m p ++ ps
branchAndBound _ [] = error "branchAndBound failed"

showSearchStep :: SearchPath -> String
showSearchStep (SearchPath p o t r maxh minh) = show t ++ "\t: At " ++ show (key p) ++ " with " ++ show r ++ " (" ++ show minh ++ "--" ++ show maxh ++ ") from valves " ++ show (Set.toList o)

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let valveMap = fst . last . P.readP_to_S parseValveMap $ input
  let Just startValve = Map.lookup "AA" valveMap
  let startHeuristic = maxReleaseHeuristic valveMap Set.empty 30
  let solution = branchAndBound valveMap [[SearchPath startValve Set.empty 30 0 startHeuristic 0]]
  mapM_ putStrLn . reverse . map showSearchStep $ solution
