import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (findIndices)
import Data.Maybe (fromMaybe)
import System.Environment
import Text.ParserCombinators.ReadP as P

data Packet = Number Int | Packet [Packet]

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


main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (packetPairs, unparsedInput) = last . readP_to_S parseInput $ input
  print . sum . map (+ 1) . findIndices (fromMaybe False . areOrdered) $ packetPairs
