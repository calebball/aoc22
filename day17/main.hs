import qualified Data.IntMap as Map
import Data.List (find)
import Data.Maybe (isJust)
import System.Environment
import Debug.Trace

data Jet = PushLeft | PushRight
  deriving Show
  
instance Read Jet where
  readsPrec _ (c:s)
    | c == '<' = [(PushLeft, s)]
    | c == '>' = [(PushRight, s)]
    | otherwise = []
  readsPrec _ [] = []
  
data Shape = Flat | Cross | Corner | Column | Block
  deriving (Show, Eq)

widthOf :: Shape -> Int
widthOf Flat = 4
widthOf Cross = 3
widthOf Corner = 3
widthOf Column = 1
widthOf Block = 2

heightOf :: Shape -> Int
heightOf Flat = 1
heightOf Cross = 3
heightOf Corner = 3
heightOf Column = 4
heightOf Block = 2

data Rock = Rock { shape :: Shape, position :: Int, height :: Int }
  deriving Show

mask :: Rock -> [[Bool]]
mask (Rock Flat p h) = [map (\n -> n >= p && n < p + 4) [0..6]]
mask (Rock Cross p h) = [map (== (p + 1)) [0..6], map (\n -> n >= p && n < p + 3) [0..6], map (== (p + 1)) [0..6]]
mask (Rock Corner p h) = [map (== (p + 2)) [0..6], map (== (p + 2)) [0..6], map (\n -> n >= p && n < p + 3) [0..6]]
mask (Rock Column p h) = replicate 4 (map (== p) [0..6])
mask (Rock Block p h) = replicate 2 (map (\n -> n == p || n == p + 1) [0..6])

pushRock :: Rock -> Jet -> Rock
pushRock (Rock s p h) PushLeft = Rock s (max 0 (p - 1)) h
pushRock (Rock s p h) PushRight = Rock s (min (7 - widthOf s) (p + 1)) h

dropRock :: Rock -> Rock
dropRock (Rock s p h) = Rock s p (h + 1)

type Chamber = [[Bool]]

collision :: Chamber -> Rock -> Bool
collision c (Rock s p h)
  | h <= 0 = False
  | h > length c = True
  | otherwise = let
                  m = reverse . take h . reverse . mask $ Rock s p h
                  window = drop (h - length m) . take h $ c
                in or . zipWith (&&) (concat m) . concat $ window
                
place :: Chamber -> Rock -> Chamber
place c (Rock s p h) = let
    m = mask (Rock s p h)
  in case compare h 0 of
    LT -> trace ("Called with " ++ show c ++ " - " ++ show (Rock s p h)) (error "Placing rock in air")
    EQ -> m ++ c
    GT -> let
        (prefixAndOverlap, suffix) = splitAt h c
        (prefix, overlap) = splitAt (h - length m) prefixAndOverlap
        (maskPrefix, maskOverlap) = splitAt (length m - h) m
      in maskPrefix ++ prefix ++ zipWith (zipWith (||)) maskOverlap overlap ++ suffix

moveRock :: [(Int, Jet)] -> Chamber -> Rock -> (Rock, [(Int, Jet)])
moveRock [] _ _ = error "Jets ended?"
moveRock (j:js) c r = let
    pushed = if collision c (pushRock r (snd j)) then r else pushRock r (snd j)
  in if collision c (dropRock pushed)
    then (pushed, js)
    else moveRock js c (dropRock pushed)

addRocks :: [Shape] -> [(Int, Jet)] -> Chamber -> Int -> Chamber
addRocks [] _ _ _ = error "Ran out of shapes?"
addRocks _ [] _ _ = error "Ran out of jets?"
addRocks _ _ c 0 = c
addRocks (s:nextShapes) js c n = let
    (newRock, nextJets) = moveRock js c (Rock s 2 (-3))
    nextChamber = place c newRock
  in addRocks nextShapes nextJets nextChamber (n - 1)
  
addRocksUntilRepeat :: [Shape] -> Map.IntMap [(Int, Rock, Chamber)] -> [(Int, Jet)] -> Int -> Chamber -> (Int, Int, Chamber)
addRocksUntilRepeat [] _ _ _ _ = error "Ran out of shapes?"
addRocksUntilRepeat _ _ [] _ _ = error "Ran out of jets?"
addRocksUntilRepeat (s:nextShapes) m ((i, j):js) n c = let
    (newRock, nextJets) = moveRock ((i, j):js) c (Rock s 2 (-3))
    nextChamber = place c newRock
    previousPlacements = Map.findWithDefault [] i m
    nextMap = Map.insert i ((n, newRock, take 10 nextChamber) : previousPlacements) m
    repeat = find (\(_, _, oldChamber) -> take 10 nextChamber == oldChamber) previousPlacements
  in if isJust repeat
    then let Just (lastRepeat, lastHeight, _) = repeat in (lastRepeat, n, nextChamber)
    else addRocksUntilRepeat nextShapes nextMap nextJets (n + 1) nextChamber

showChamber :: Chamber -> String
showChamber [] = "+-------+"
showChamber (c:cs) = "|" ++ map (\b -> if b then '#' else ' ') c ++ "|\n" ++ showChamber cs

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let shapes = cycle [Flat, Cross, Corner, Column, Block]
  let jets = cycle . zip [0..] . map (read . pure) . head . lines $ input :: [(Int, Jet)]
  
  let (repeatStart, repeatEnd, chamber) = addRocksUntilRepeat shapes Map.empty jets 1 []
  let repeatPeriod = repeatEnd - repeatStart
  
  print (repeatStart, repeatEnd, repeatPeriod)
  putStrLn . showChamber . take 10 . addRocks shapes jets [] $ repeatStart
  putStrLn . showChamber . take 10 . addRocks shapes jets [] $ repeatEnd
  putStrLn . showChamber . take 10 . addRocks shapes jets [] $ repeatEnd + repeatPeriod
  
  let repeatStartHeight = length . addRocks shapes jets [] $ repeatStart
  let repeatEndHeight = length . addRocks shapes jets [] $ repeatEnd
  let heightGain = repeatEndHeight - repeatStartHeight
  
  let (numRepeats, remainingPlacements) = 1000000000000 `quotRem` repeatPeriod
  
  print (heightGain * numRepeats)
  print (length . addRocks shapes jets [] $ remainingPlacements)
  print ((heightGain * numRepeats) + (length . addRocks shapes jets [] $ remainingPlacements))
