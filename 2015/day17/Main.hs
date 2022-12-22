module Main where
import System.Environment (getArgs)


combinations :: Int -> [Int] -> [[Int]]
combinations total xs  = run total xs
  where run 0   _       = [[]]
        run amt []      = []
        run amt (x:xs)  = 
          (run amt xs) ++ (if x <= amt then map (x:) $ run (amt - x) xs else [])


main = do
  [filename] <- getArgs
  contents <- readFile filename
  let containers :: [Int] = map read $ lines contents
  let combos = combinations 150 containers
  let min = minimum $ map length combos
  print $ length $ combos
  print $ length $ filter ((==) min . length) $ combos


