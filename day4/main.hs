import System.Environment
import Data.Char
import Data.List
import Data.Ix
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

newtype Assignment = Assignment (Int, Int)
  deriving Show

parseInt :: ReadP Int
parseInt = do 
  d <- munch1 isDigit
  maybe pfail return (readMaybe d)

parseAssignment :: ReadP Assignment
parseAssignment = do
  start <- parseInt
  char '-'
  end <- parseInt
  return (Assignment (start, end))

parseAssignmentPair :: ReadP (Assignment, Assignment)
parseAssignmentPair = do
  skipSpaces
  first <- parseAssignment
  char ','
  second <- parseAssignment
  return (first, second)

sections :: Assignment -> [Int]
sections (Assignment p) = range p

containedIn :: Assignment -> Assignment -> Bool
containedIn a b = sections a `isSubsequenceOf` sections b

isRedundantWith :: Assignment -> Assignment -> Bool
isRedundantWith a b = (a `containedIn` b) || (b `containedIn` a)

hasOverlapWith :: Assignment -> Assignment -> Bool
hasOverlapWith a b = not (null (sections a `intersect` sections b))

main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let parse_results = last (readP_to_S (many1 parseAssignmentPair) input)
  let assignment_pairs = fst parse_results
  print (length (filter (uncurry hasOverlapWith) assignment_pairs))
