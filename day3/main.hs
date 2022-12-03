import Data.Char
import Data.List
import System.Environment

type Item = Char
  
priority :: Item -> Int
priority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
  | otherwise = 0


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
  
allItems :: Rucksack -> [Item]
allItems (Rucksack f b) = f ++ b
  

data Group = Group Rucksack Rucksack Rucksack
  deriving Show

fromRucksacks :: [Rucksack] -> [Group]
fromRucksacks [] = []
fromRucksacks (r1:r2:r3:rs) = Group r1 r2 r3 : fromRucksacks rs
fromRucksacks _ = []
  
badge :: Group -> Item
badge (Group r1 r2 r3) = head (allItems r1 `intersect` allItems r2 `intersect` allItems r3)


main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let rucksacks = map read (lines input) :: [Rucksack]
  let mistakes = map incorrectItems rucksacks
  let priorities = map (sum . map priority) mistakes
  let groups = fromRucksacks rucksacks
  let badges = map badge groups
  print (sum (map priority badges))