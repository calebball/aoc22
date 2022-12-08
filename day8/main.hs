import System.Environment
import Data.Char (digitToInt)
import Data.List (transpose)

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

main = do
  inputPath <- head <$> getArgs
  input <- readFile inputPath
  let rows = map (map digitToInt) . lines $ input :: [[Int]]
  let horizontal = map isTreeVisible rows
  let vertical = transpose . map isTreeVisible . transpose $ rows
  let visibleTrees = zipWith (||) (concat horizontal) (concat vertical)
  print . sum . map fromEnum $ visibleTrees
