import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map as Map
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.ReadP as P
import Debug.Trace

type Item = Int

data Monkey = Monkey {
  number :: Int,
  items :: [Item],
  operation :: Int -> Int,
  test :: Int -> Bool,
  targetWhenTrue :: Int,
  targetWhenFalse :: Int,
  inspections :: Int
}

instance Show Monkey where
  show m = "Monkey " ++ show (number m) ++ ": " ++ show (items m)

parseOp :: ReadP (Int -> Int -> Int)
parseOp = readAdd <|> readMult
  where
    readAdd = P.char '+' $> (+)
    readMult = P.char '*' $> (*)

parseItems :: ReadP [Int]
parseItems = do
  P.string "Starting items: "
  map read <$> P.sepBy (P.munch1 isDigit) (P.string ", ")
  
parseOperation :: ReadP (Int -> Int)
parseOperation = do
  P.string "Operation: new = old "
  op <- parseOp
  P.skipSpaces
  right <- P.string "old" <|> P.munch1 isDigit
  case right of
    "old" -> return (\n -> op n n)
    d -> return (op (read d))
    
parseTest :: ReadP (Int -> Bool)
parseTest = do
  P.string "Test: divisible by "
  value <- read <$> P.munch1 isDigit
  return (\n -> n `mod` value == 0)
  
parseWhenTrue :: ReadP Int
parseWhenTrue = do
  P.string "If true: throw to monkey "
  read <$> P.munch1 isDigit

parseWhenFalse :: ReadP Int
parseWhenFalse = do
  P.string "If false: throw to monkey "
  read <$> P.munch1 isDigit
  
parseMonkey :: ReadP Monkey
parseMonkey = do
  P.string "Monkey "
  number <- read <$> P.munch1 isDigit
  P.char ':'
  P.skipSpaces
  items <- parseItems
  P.skipSpaces
  operation <- parseOperation
  P.skipSpaces
  test <- parseTest
  P.skipSpaces
  whenTrue <- parseWhenTrue
  P.skipSpaces
  whenFalse <- parseWhenFalse
  return (Monkey number items operation test whenTrue whenFalse 0)
  
parseInput :: ReadP [Monkey]
parseInput = P.sepBy parseMonkey P.skipSpaces

throwItem :: Map.Map Int Monkey -> Int -> Item -> Map.Map Int Monkey
throwItem ms target item = Map.adjust addItem target ms
  where addItem (Monkey n items op t wt wf ins) = Monkey n (item:items) op t wt wf ins

inspectItems :: Map.Map Int Monkey -> Int -> Map.Map Int Monkey
inspectItems ms n = let
    Monkey _ items op t wt wf ins = ms Map.! n
    inspected = map ((\v -> (bool wf wt (t v), v)) . (`div` 3) . op) items
  in
    Map.insert n (Monkey n [] op t wt wf (ins + length inspected)) . foldl (\ms (t, i) -> throwItem ms t i) ms $ inspected
    
keepAwayRound :: Map.Map Int Monkey -> Map.Map Int Monkey
keepAwayRound ms = foldl inspectItems ms [0 .. (maximum . Map.keys $ ms)]

solvePart1 :: Map.Map Int Monkey -> Int
solvePart1 ms = let
    round = iterate keepAwayRound ms
  in
    product . take 2 . reverse . sort . Map.elems . Map.map inspections $ round !! 20

main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      input <- readFile inputFile
      let parsedInput = fst . last . P.readP_to_S parseInput $ input
      let monkeys = Map.fromList . map (\m -> (number m, m)) $ parsedInput
      print (solvePart1 monkeys)
      
    _failure -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " [input file]"
      exitFailure
