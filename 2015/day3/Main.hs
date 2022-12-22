module Main where
import System.Environment (getArgs)
import Data.List (nub)

data House = H { x :: Integer, y :: Integer} deriving (Eq, Ord, Show)

parse :: String -> [House]
parse contents = walk contents 0 0 []
  where walk ""       x y acc = (H x y):acc
        walk ('^':cs) x y acc = walk cs x (y+1) ((H x y):acc)
        walk ('v':cs) x y acc = walk cs x (y-1) ((H x y):acc)
        walk ('<':cs) x y acc = walk cs (x-1) y ((H x y):acc)
        walk ('>':cs) x y acc = walk cs (x+1) y ((H x y):acc)

alternate :: [a] -> ([a], [a])
alternate cs = go cs [] []
  where go []       s r = (reverse s, reverse r)
        go [x]      s r = (reverse (x:s), reverse r)
        go (a:b:cs) s r = go cs (a:s) (b:r)

process1 :: String -> Int
process1 contents = length $ nub $ parse contents

process2 :: String -> Int
process2 contents = length $ nub $ (parse santa ++ parse robot)
  where (santa, robot) = alternate contents

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ process1 contents
  print $ process2 contents