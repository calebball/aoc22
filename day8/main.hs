import Data.Char (digitToInt)
import Data.List (transpose, inits, tails, findIndex)
import System.Environment

isTreeVisibleL :: [Int] -> [Bool]
isTreeVisibleL [] = []
isTreeVisibleL (x:xs) = map snd . scanl isVisible (x, True) $ xs
  where isVisible (currentHeight, _) nextHeight
          | nextHeight > currentHeight = (nextHeight, True)
          | otherwise = (currentHeight, False)

isTreeVisibleR :: [Int] -> [Bool]
isTreeVisibleR [] = []
isTreeVisibleR l = map snd . scanr isVisible (last l, True) $ init l
  where isVisible nextHeight (currentHeight, _)
          | nextHeight > currentHeight = (nextHeight, True)
          | otherwise = (currentHeight, False)
          
isTreeVisible :: [Int] -> [Bool]
isTreeVisible l = zipWith (||) (isTreeVisibleL l) (isTreeVisibleR l)

solvePart1 :: [[Int]] -> Int
solvePart1 rows = let
    horizontal = map isTreeVisible rows
    vertical = transpose . map isTreeVisible . transpose $ rows
    visibleTrees = zipWith (||) (concat horizontal) (concat vertical)
  in
    sum . map fromEnum $ visibleTrees
    
    
visibleFromTree :: Int -> [Int] -> Int
visibleFromTree t [] = 0
visibleFromTree t l = case findIndex (>= t) l of
  Just i -> i + 1
  Nothing -> length l

visibleWithinRow :: [Int] -> [Int]
visibleWithinRow l = zipWith3 innerFn l (init . inits $ l) (tail . tails $ l)
  where innerFn t leftOf rightOf = visibleFromTree t (reverse leftOf) * visibleFromTree t rightOf

solvePart2 :: [[Int]] -> Int
solvePart2 rows = let
    horizontal = map visibleWithinRow rows
    vertical = transpose . map visibleWithinRow . transpose $ rows
    visibleWithinForest = zipWith (*) (concat horizontal) (concat vertical)
  in
    maximum visibleWithinForest


main = do
  inputPath <- head <$> getArgs
  input <- readFile inputPath
  let rows = map (map digitToInt) . lines $ input
  print . solvePart1 $ rows
  print . solvePart2 $ rows
