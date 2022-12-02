import System.Environment
import Data.List

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = f xs []
  where f [] acc = [acc]
        f (x:xs) acc = if p x then acc : f xs [] else f xs (x : acc)

main = do
  args <- getArgs
  let input_path = head args
  input <- readFile input_path
  let groups = splitWhen null (lines input)
  let parsed_groups = map (map read) groups
  let sorted_groups = reverse (sort (map sum parsed_groups))
  print (sum (take 3 sorted_groups))
