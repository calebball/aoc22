import Control.Applicative ((<|>))
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import Text.ParserCombinators.ReadP

type Crate = Char

type Deck = [[Crate]]

deckFromCrateRows :: [[Maybe Crate]] -> [[Crate]]
deckFromCrateRows rows = map catMaybes (transpose rows)

data Move = Move { source :: Int, destination :: Int, number :: Int }
  deriving Show
  
applyMove :: Deck -> Move -> Deck
applyMove deck move = let
    (beforeS, oldS:afterS) = splitAt (source move) deck
    (moving, newS) = splitAt (number move) oldS
    newDeck = beforeS ++ [newS] ++ afterS
    (beforeD, oldD:afterD) = splitAt (destination move) newDeck
    newD = reverse moving ++ oldD
  in
    beforeD ++ [newD] ++ afterD

parseCrate :: ReadP (Maybe Crate)
parseCrate = let
    occupied = do
      char '['
      c <- satisfy isAsciiUpper
      char ']'
      return (Just c)
    unoccupied = do
      count 3 (char ' ')
      return Nothing
  in do occupied <|> unoccupied
  
parseCrateRow :: ReadP [Maybe Crate]
parseCrateRow = sepBy1 parseCrate (char ' ')

parseShipDeck :: ReadP [[Crate]]
parseShipDeck = do
  rows <- sepBy1 parseCrateRow (char '\n')
  many (do
    skipSpaces
    satisfy isDigit
    skipSpaces)
  return (deckFromCrateRows rows)
  
parseMovement :: ReadP Move
parseMovement = do
  string "move "
  number <- munch1 isDigit
  string " from "
  source <- munch1 isDigit
  string " to "
  destination <- munch1 isDigit
  return (Move (read source - 1) (read destination - 1) (read number))
  
parseMovementList :: ReadP [Move]
parseMovementList = sepBy parseMovement (char '\n')
  
parseInput :: ReadP (Deck, [Move])
parseInput = do
  deck <- parseShipDeck
  skipSpaces
  moves <- parseMovementList
  skipSpaces
  eof
  return (deck, moves)
  
main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let (deck, moves) = fst (last (readP_to_S parseInput input))
  let shuffled = foldl applyMove deck moves
  print (map head shuffled)
