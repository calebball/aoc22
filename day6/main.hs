import Data.List (findIndex)
import Data.Maybe (fromJust)
import System.Environment

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows size (x:xs)
  | length (x:xs) < size = []
  | otherwise = take size (x:xs) : windows size xs

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

solve :: Int -> String -> Maybe Int
solve size = fmap (size +) . findIndex unique. windows size

main = do
  (size:filePath:_) <- getArgs
  input <- readFile filePath
  print . solve (read size) $ input
