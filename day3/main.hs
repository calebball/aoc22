import Data.Char
import Data.List
import System.Environment

type Item = Char

type FrontCompartment = [Item]
type BackCompartment = [Item]

data Rucksack = Rucksack FrontCompartment BackCompartment
  deriving Show

instance Read Rucksack where
  readsPrec _ i =
    let
      (firstHalf, secondHalf) = splitAt (length i `div` 2) i
    in
      [(Rucksack firstHalf secondHalf, "")]
      
incorrectItems :: Rucksack -> [Item]
incorrectItems (Rucksack f b) =
  nub (filter (`elem` b) f)
  
priority :: Item -> Int
priority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
  | otherwise = 0

main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let rucksacks = map read (lines input) :: [Rucksack]
  let mistakes = map incorrectItems rucksacks
  let priorities = map (sum . map priority) mistakes
  print (sum priorities)
