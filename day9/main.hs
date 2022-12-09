import Data.Set (fromList, size)
import System.Environment


data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving Show

instance Read Direction where
  readsPrec _ [] = []
  readsPrec _ (d:s) = case d of
    'U' -> [(MoveUp, s)]
    'D' -> [(MoveDown, s)]
    'L' -> [(MoveLeft, s)]
    'R' -> [(MoveRight, s)]
    _ -> []
  
data Movement = Movement { direction :: Direction, repeats :: Int }
  deriving Show

readMovement :: String -> Movement
readMovement s = case words s of
  [d, r] -> Movement (read d) (read r)
  
expandMovement :: Movement -> [Direction]
expandMovement m = replicate (repeats m) (direction m)
  
  
type Position = (Int, Int)

move :: Position -> Direction -> Position
move (x, y) d = case d of
  MoveUp -> (x, y + 1)
  MoveDown -> (x, y - 1)
  MoveLeft -> (x - 1, y)
  MoveRight -> (x + 1, y)

data Rope = Rope { ropeHead :: Position, ropeTail :: Position }
  deriving Show

moveInDirection :: Rope -> Direction -> Rope
moveInDirection (Rope h (tx, ty)) d
  | tx == hx && hy - ty > 1 = Rope (hx, hy) (tx, ty + 1)
  | tx == hx && ty - hy > 1 = Rope (hx, hy) (tx, ty - 1)
  | ty == hy && hx - tx > 1 = Rope (hx, hy) (tx + 1, ty)
  | ty == hy && tx - hx > 1 = Rope (hx, hy) (tx - 1, ty)
  | tx < hx && hy - ty > 1 = Rope (hx, hy) (tx + 1, ty + 1)
  | tx < hx && ty - hy > 1 = Rope (hx, hy) (tx + 1, ty - 1)
  | ty < hy && hx - tx > 1 = Rope (hx, hy) (tx + 1, ty + 1)
  | ty < hy && tx - hx > 1 = Rope (hx, hy) (tx - 1, ty + 1)
  | tx > hx && hy - ty > 1 = Rope (hx, hy) (tx - 1, ty + 1)
  | tx > hx && ty - hy > 1 = Rope (hx, hy) (tx - 1, ty - 1)
  | ty > hy && hx - tx > 1 = Rope (hx, hy) (tx + 1, ty - 1)
  | ty > hy && tx - hx > 1 = Rope (hx, hy) (tx - 1, ty - 1)
  | otherwise = Rope (hx, hy) (tx, ty)
  where (hx, hy) = move h d


main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let movements = concatMap (expandMovement . readMovement) . lines $ input
  let positions = scanl moveInDirection (Rope (0, 0) (0, 0)) movements
  print . size . fromList . map ropeTail $ positions
  -- mapM_ putStrLn (zipWith (\m p -> (drop 4 . show $ m) ++ "\t" ++ show p) movements positions)
