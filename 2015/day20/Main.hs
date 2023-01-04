module Main where
import System.Environment (getArgs)

house :: Int -> Int
house n = sum $ map (*10) $ filter ((== 0) . (n `mod`) ) [1..n]

main = do
  n <- read . head <$> getArgs
  print $ head $ dropWhile ((< n) . snd) $  map (\x -> (x, house x)) [1..]