import Control.Applicative ((<|>))
import Data.Char (isSpace, isDigit)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Sequence (fromList, chunksOf)
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.ReadP as P
import Text.Read (readMaybe)

newtype Registers = Registers { regX :: Int }
  deriving Show

data Instruction = Noop | Addx Int
  deriving Show
  
execute :: [Registers] -> Instruction -> [Registers]
execute rs Noop = rs ++ [last rs]
execute rs (Addx i) = let r = last rs in rs ++ [r, Registers (regX r + i)]

spriteVisible :: Registers -> Int -> Bool
spriteVisible (Registers x) i = let
    pos = i `mod` 40
  in
    abs (x - pos) < 2

parseNoop :: P.ReadP Instruction
parseNoop = do
  P.string "noop"
  return Noop

parseAddx :: P.ReadP Instruction
parseAddx = do
  P.string "addx"
  P.skipMany1 (P.satisfy isSpace)
  sign <- P.option "" (P.string "-")
  value <- P.munch1 isDigit
  case readMaybe (sign ++ value) of
    Just i -> return (Addx i)
    Nothing -> P.pfail
    
parseInstruction :: P.ReadP Instruction
parseInstruction = parseNoop <|> parseAddx

parseInput :: P.ReadP [Instruction]
parseInput = do
  result <- P.sepBy parseInstruction (P.char '\n')
  P.char '\n'
  P.eof
  return result
  
newtype Display = Display [Bool]

instance Show Display where
  show (Display pixels) = let
      rows = map toList . toList . chunksOf 40 . fromList $ pixels
    in
      intercalate "\n" . map (map pixelToChar) $ rows
    where pixelToChar b = if b then '#' else '.'
  
solvePart1 :: [Instruction] -> Int
solvePart1 instructions = let
    regs = foldl execute [Registers 1] instructions
    indices = [20, 60, 100, 140, 180, 220]
    signalStrength i = regX (regs !! (i - 1)) * i
  in
    sum . map signalStrength $ indices
    
solvePart2 :: [Instruction] -> Display
solvePart2 instructions = let
    regs = foldl execute [Registers 1] instructions
  in
    Display (zipWith spriteVisible regs (iterate (+ 1) 0))

main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      input <- readFile inputFile
      case P.readP_to_S parseInput input of
        [(instructions, "")] -> do
          print . solvePart2 $ instructions

        _parseFailure -> do
          hPutStrLn stderr "Failed to parse input file"
          exitFailure
    
    _argsFailure -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " [input file]"
      exitFailure
