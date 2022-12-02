import System.Environment
import Text.Read

class Score a where
  score :: a -> Int


data Hand = Rock | Paper | Scissors
  deriving (Show, Eq)
  
instance Score Hand where
  score Rock = 1
  score Paper = 2
  score Scissors = 3
  

newtype OpponentHand = OpponentHand Hand
  deriving Show

instance Read OpponentHand where
  readsPrec _ i = case i of
    'A':s -> [(OpponentHand Rock, s)]
    'B':s -> [(OpponentHand Paper, s)]
    'C':s -> [(OpponentHand Scissors, s)]
    _ -> []
    
instance Score OpponentHand where
  score (OpponentHand h) = score h


newtype MyHand = MyHand Hand
  deriving Show

instance Read MyHand where
  readsPrec _ i = case i of
    'X':s -> [(MyHand Rock, s)]
    'Y':s -> [(MyHand Paper, s)]
    'Z':s -> [(MyHand Scissors, s)]
    _ -> []
    
instance Score MyHand where
  score (MyHand h) = score h

    
data Outcome = Win | Lose | Draw
  deriving Show
  
instance Read Outcome where
  readsPrec _ i = case i of
    'X':s -> [(Lose, s)]
    'Y':s -> [(Draw, s)]
    'Z':s -> [(Win, s)]
    _ -> []
    
instance Score Outcome where
  score Win = 6
  score Draw = 3
  score Lose = 0
  

winsAgainst :: Hand -> Hand
winsAgainst Rock = Paper
winsAgainst Paper = Scissors
winsAgainst Scissors = Rock

drawsAgainst :: Hand -> Hand
drawsAgainst h = h

losesAgainst :: Hand -> Hand
losesAgainst Rock = Scissors
losesAgainst Paper = Rock
losesAgainst Scissors = Paper

matchingHand :: Hand -> Outcome -> Hand
matchingHand h Win = winsAgainst h
matchingHand h Draw = drawsAgainst h
matchingHand h Lose = losesAgainst h
  

data RiggedGame = RiggedGame OpponentHand Outcome
  deriving Show
  
instance Read RiggedGame where
  readsPrec _ (h:' ':o:s) = 
    let
      hand = (read [h] :: OpponentHand)
      outcome = (read [o] :: Outcome)
    in
      [(RiggedGame hand outcome, s)]
  readsPrec _ _ = []
  
instance Score RiggedGame where
  score (RiggedGame (OpponentHand h) o) = score (matchingHand h o) + score o
   

data Game = Game MyHand OpponentHand
  deriving Show
  
instance Read Game where
  readsPrec _ (o:' ':m:s) = 
    let
      op = (read [o] :: OpponentHand)
      my = (read [m] :: MyHand)
    in
      [(Game my op, s)]
  readsPrec _ _ = []

instance Score Game where
  score (Game (MyHand m) (OpponentHand o))
    | m == winsAgainst o  = score Win + score m
    | m == drawsAgainst o = score Draw + score m
    | m == losesAgainst o = score Lose + score m
    | otherwise = 0
  

main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let games = map read (lines input) :: [RiggedGame]
  print (sum (map score games))
