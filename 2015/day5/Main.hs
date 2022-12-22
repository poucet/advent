module Main where
import System.Environment (getArgs)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel  _  = False

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c l = go l []
  where go [] []                  = []
        go [] acc                 = [reverse  acc]
        go (x:xs) acc | x == c    = (reverse acc):(go xs [])
        go (x:xs) acc | otherwise = (go xs (x:acc))

contains :: Eq a => [a] -> [a] -> Bool
contains [] _                       = True
contains (x:xs) []                  = False
contains (x:xs) (y:ys) | x == y     = isPrefix xs ys || contains (x:xs) ys
                       | otherwise  = contains (x:xs) ys
  where isPrefix [] _           = True
        isPrefix _ []           = False
        isPrefix (x:xs) (y:ys)  = x == y && isPrefix xs ys

containsDouble :: Eq a => [a] -> Bool
containsDouble []       =  False
containsDouble [x]      = False
containsDouble (x:y:xs) = (x == y) || containsDouble (y:xs)

containsXYX :: Eq a => [a] -> Bool
containsXYX (x:y:z:xs) | x == z     = True
                       | otherwise  = containsXYX (y:z:xs)
containsXYX _                       = False

containsRepeatedPair []       = False
containsRepeatedPair [x]      = False
containsRepeatedPair (x:y:xs) = contains [x, y] xs || containsRepeatedPair (y:xs)

isNice1 :: String -> Bool
isNice1 input = hasThreeVowels && doesNotContain ["ab", "cd", "pq", "xy"] && containsDouble input
  where hasThreeVowels = (length $ filter isVowel input) >= 3 
        doesNotContain l = (filter (\x -> contains x input) l) == []

isNice2 :: String -> Bool
isNice2 input = containsXYX input && containsRepeatedPair input


main = do
  [filename] <- getArgs
  contents <- readFile filename
  let strings = splitOn '\n' contents
  print $ length $ filter (\x -> isNice1 x) strings
  print $ length $ filter (\x -> isNice2 x) strings