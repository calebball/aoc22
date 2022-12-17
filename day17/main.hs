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

placeRock :: (Chamber, [Jet]) -> Rock -> (Chamber, [Jet])
placeRock (_, []) _ = error "Jets ended?"
placeRock (c, j:js) r = let
    pushed = if collision c (pushRock r j) then r else pushRock r j
  in if collision c (dropRock pushed)
    then (place c pushed, js)
    else placeRock (c, js) (dropRock pushed)

addRocks :: [Shape] -> (Chamber, [Jet]) -> [(Chamber, [Jet])]
addRocks [] _ = error "Ran out of shapes?"
addRocks (s:nextShapes) state = let
    nextState = placeRock state (Rock s 2 (-3))
  in nextState : addRocks nextShapes nextState

showChamber :: Chamber -> String
showChamber [] = "+-------+"
showChamber (c:cs) = "|" ++ map (\b -> if b then '#' else ' ') c ++ "|\n" ++ showChamber cs

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let jets = map (read . pure) (head . lines $ input) :: [Jet]
  let chamber = []
  let states = addRocks (cycle [Flat, Cross, Corner, Column, Block]) (chamber, cycle jets)
  putStrLn . showChamber . fst $ states !! 2021
  print . length . fst $ states !! 2021
