import Control.Monad (unless)
import Data.Either
import Data.Set (fromList, size)
import System.Environment
import System.Exit
import System.IO
import Text.Read (readMaybe)


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
  
readMovement :: String -> Either String [Direction]
readMovement s = case words s of
  [direction, repeats] -> case (readMaybe direction, readMaybe repeats) of
    (Just d, Just r) -> Right (replicate r d)
    (Nothing, Just _) -> Left ("The string '" ++ direction ++ "' is not a valid direction.")
    (Just _, Nothing) -> Left ("The string '" ++ repeats ++ "' is not a valid number of repeats.")
    (Nothing, Nothing) -> Left ("The string '" ++ s ++ "' is not a valid movement input.")
  _failure -> Left ("The string '" ++ s ++ "' is not a valid movement input.")
  
  
type Knot = (Int, Int)

move :: Direction -> Knot -> Knot
move d (x, y) = case d of
  MoveUp -> (x, y + 1)
  MoveDown -> (x, y - 1)
  MoveLeft -> (x - 1, y)
  MoveRight -> (x + 1, y)

follow :: Knot -> Knot -> Knot
follow (hx, hy) (tx, ty)
  | tx == hx && hy - ty > 1 = (tx, ty + 1)
  | tx == hx && ty - hy > 1 = (tx, ty - 1)
  | ty == hy && hx - tx > 1 = (tx + 1, ty)
  | ty == hy && tx - hx > 1 = (tx - 1, ty)
  | tx < hx && hy - ty > 1  = (tx + 1, ty + 1)
  | tx < hx && ty - hy > 1  = (tx + 1, ty - 1)
  | ty < hy && hx - tx > 1  = (tx + 1, ty + 1)
  | ty < hy && tx - hx > 1  = (tx - 1, ty + 1)
  | tx > hx && hy - ty > 1  = (tx - 1, ty + 1)
  | tx > hx && ty - hy > 1  = (tx - 1, ty - 1)
  | ty > hy && hx - tx > 1  = (tx + 1, ty - 1)
  | ty > hy && tx - hx > 1  = (tx - 1, ty - 1)
  | otherwise               = (tx, ty)

type Rope = [Knot]

moveRope :: Rope -> Direction -> Rope
moveRope [] _ = []
moveRope (h:t) d = scanl follow (move d h) t


main = do
  args <- getArgs
  case args of
    [knots, inputFile] | Just k <- readMaybe knots -> do
      unless (k > 0) (do
        hPutStrLn stderr "The number of knots must be greater than 0"
        exitFailure)

      input <- readFile inputFile
      let parsingResults = map readMovement . lines $ input
      unless (null . lefts $ parsingResults) (do
        hPutStrLn stderr "Failed while parsing input file:"
        mapM_ (hPutStrLn stderr) (lefts parsingResults)
        exitFailure)
          
      let movements = concat . rights $ parsingResults
      let positions = scanl moveRope (replicate k (0, 0)) movements
      print . size . fromList . map last $ positions
      
    _failure -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " [number of knots] [input file]"
      exitFailure
