import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (elemIndex, findIndices, sortBy)
import Data.Maybe (fromMaybe, fromJust)
import System.Environment
import Text.ParserCombinators.ReadP as P
import Debug.Trace

data Packet = Number Int | Packet [Packet]
  deriving Eq

instance Show Packet where
  show (Number n) = show n
  show (Packet l) = show l

parseNumber :: P.ReadP Packet
parseNumber = Number . read <$> P.munch1 isDigit

parseList :: P.ReadP Packet
parseList = Packet <$> P.between (P.char '[') (P.char ']') (P.sepBy parsePacket (P.char ','))

parsePacket :: P.ReadP Packet
parsePacket = parseList <|> parseNumber
  
parsePacketPair :: P.ReadP (Packet, Packet)
parsePacketPair = do
  left <- parseList
  P.char '\n'
  right <- parseList
  return (left, right)
  
parseInput :: P.ReadP [(Packet, Packet)]
parseInput = P.sepBy parsePacketPair (P.string "\n\n")

areOrdered :: (Packet, Packet) -> Maybe Bool
areOrdered (Number a, Number b)
  | a < b = Just True
  | a > b = Just False
  | otherwise = Nothing
areOrdered (Number a, Packet bs) = areOrdered (Packet [Number a], Packet bs)
areOrdered (Packet as, Number b) = areOrdered (Packet as, Packet [Number b])
areOrdered (Packet [], Packet []) = Nothing
areOrdered (Packet [], Packet _) = Just True
areOrdered (Packet _, Packet []) = Just False
areOrdered (Packet (a:as), Packet (b:bs)) = case areOrdered (a, b) of
  Nothing -> areOrdered (Packet as, Packet bs)
  Just b -> Just b
  
packetCompare :: Packet -> Packet -> Ordering
packetCompare a b = case areOrdered (a, b) of
  Just True -> LT
  Just False -> GT
  Nothing -> EQ


solvePart1 :: [(Packet, Packet)] -> Int
solvePart1 = sum . map (+ 1) . findIndices (fromMaybe False . areOrdered)

solvePart2 :: [Packet] -> Int
solvePart2 ps = let
    sorted = sortBy packetCompare ps
    firstDivider = Packet [Packet [Number 2]]
    secondDivider = Packet [Packet [Number 6]]
  in
    (fromJust (elemIndex firstDivider sorted) + 1) * (fromJust (elemIndex secondDivider sorted) + 1)

main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (packetPairs, unparsedInput) = last . readP_to_S parseInput $ input
  let packets = [Packet [Packet [Number 2]], Packet [Packet [Number 6]]] ++ uncurry (++) (unzip packetPairs)
  print . solvePart1 $ packetPairs
  print . solvePart2 $ packets
