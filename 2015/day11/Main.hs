module Main where
import Data.Char
import System.Environment (getArgs)

next = reverse . next' . reverse 
  where next' []                = []
        next' (x:xs) | x == 'z' = 'a':next' xs
                     | otherwise = (succ x):xs


hasPairs 0 _                    = True
hasPairs n []                   = False
hasPairs n [x]                  = False
hasPairs n (x:y:xs) | x == y    = hasPairs (n-1) xs
                    | otherwise = hasPairs n (y:xs)

hasStraight :: (Eq a, Enum a) => [a] -> Bool
hasStraight []          = False
hasStraight [x]         = False
hasStraight [x, y]      = False
hasStraight (a:b:c:xs)  = (succ a == b && succ b == c) || hasStraight (b:c:xs)

isValid :: String -> Bool
isValid input = no "iol" && hasPairs 2 input && hasStraight input
  where no cs = and $ map (flip notElem input) cs

problem1 :: String -> String 
problem1 input = head $ dropWhile (not . isValid) $ iterate next input

main = do
  [input] <- getArgs
  print $ problem1 input
  print $ problem1 . next . problem1 $ input
