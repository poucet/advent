module Main where
import Data.List (group)

lookAndSay 0 input = input
lookAndSay n input = lookAndSay (n - 1) $ apply input
  where apply str = concatMap (\x -> (show . length $ x) ++ (take 1 x)) $ group str


problem1 :: Integer-> String -> IO ()
problem1 n input = print $ length  $ lookAndSay n input


main = do
  problem1 40 "1113122113"
  problem1 50 "1113122113"