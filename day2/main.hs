import System.Environment
import Text.Read

data Hand = Rock | Paper | Scissors
  deriving Show

newtype OpponentHand = OpponentHand Hand
  deriving Show

instance Read OpponentHand where
  readsPrec _ i = case i of
    'A':s -> [(OpponentHand Rock, s)]
    'B':s -> [(OpponentHand Paper, s)]
    'C':s -> [(OpponentHand Scissors, s)]
    _ -> []

newtype MyHand = MyHand Hand
  deriving Show

instance Read MyHand where
  readsPrec _ i = case i of
    'X':s -> [(MyHand Rock, s)]
    'Y':s -> [(MyHand Paper, s)]
    'Z':s -> [(MyHand Scissors, s)]
    _ -> []
    
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
  
score :: Game -> Int
score (Game (MyHand Rock)     (OpponentHand Rock))     = 3 + 1
score (Game (MyHand Rock)     (OpponentHand Paper))    = 0 + 1
score (Game (MyHand Rock)     (OpponentHand Scissors)) = 6 + 1
score (Game (MyHand Paper)    (OpponentHand Rock))     = 6 + 2
score (Game (MyHand Paper)    (OpponentHand Paper))    = 3 + 2
score (Game (MyHand Paper)    (OpponentHand Scissors)) = 0 + 2
score (Game (MyHand Scissors) (OpponentHand Rock))     = 0 + 3
score (Game (MyHand Scissors) (OpponentHand Paper))    = 6 + 3
score (Game (MyHand Scissors) (OpponentHand Scissors)) = 3 + 3

main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let games = map read (lines input) :: [Game]
  print (sum (map score games))
