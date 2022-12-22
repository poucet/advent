module Main where
import Data.Maybe (catMaybes)
import System.Environment (getArgs)


data Box = Box {l :: Integer, w :: Integer, h :: Integer}
  deriving (Show)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c l = go l []
  where go [] []                  = []
        go [] acc                 = [reverse  acc]
        go (x:xs) acc | x == c    = (reverse acc):(go xs [])
        go (x:xs) acc | otherwise = (go xs (x:acc))

parse :: String -> Maybe Box
parse input = go (splitOn 'x' input)
  where go [a, b, c] = Just $ Box (read a) (read b) (read c)
        go _          = Nothing

paper :: Box -> Integer
paper (Box l w h) = (2*l*w) + (2*w*h) + (2*h*l) + (min (min (l*w) (h*l)) (w*h))

ribbon :: Box -> Integer
ribbon (Box l w h) = l*w*h + wrap l w h
  where wrap l w h | l <= h && w <= h = 2 * l + 2 * w
        wrap l w h | l > h = wrap h w l
        wrap l w h | w > h = wrap l h w 

process1 :: String -> Integer
process1 contents = sum $ map paper $  catMaybes $ map parse $ splitOn '\n' contents

process2 :: String -> Integer
process2 contents = sum $ map ribbon $  catMaybes $ map parse $ splitOn '\n' contents

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ process1 contents
  print $ process2 contents