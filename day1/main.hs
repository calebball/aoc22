import System.Environment

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
  print (maximum (map sum parsed_groups))
