module Main where
import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP (ReadP, string, between, readS_to_P, sepBy, readP_to_S)


data JSON =
  S String |
  N Integer |
  O [(String, JSON)] |
  A [JSON] deriving (Eq, Show)


p :: ReadP JSON
p =
  O <$> between (string "{") (string "}") (pEntry `sepBy` (string ",")) <|>
  A <$> between (string "[") (string "]") (p `sepBy` (string ",")) <|>
  S <$> pString <|>
  N <$> readS_to_P reads

sumJSON :: JSON -> Integer
sumJSON (S _) = 0
sumJSON (N n) = n
sumJSON (O p) = sum $ map (sumJSON . snd) p
sumJSON (A a) = sum $ map sumJSON a

sumJSON2 :: JSON -> Integer
sumJSON2 (S _) = 0
sumJSON2 (N n) = n
sumJSON2 (O p) | any ((== (S "red")) . snd) p = 0
sumJSON2 (O p) | otherwise = sum $ map (sumJSON2 . snd) p
sumJSON2 (A a) = sum $ map sumJSON2 a


pString :: ReadP String
pString = readS_to_P reads

pEntry :: ReadP (String, JSON)
pEntry = do
  name <- pString 
  (string ":") 
  value <- p
  return (name, value)

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ sumJSON $ fst $ head $ readP_to_S p contents
  print $ sumJSON2 $ fst $ head $ readP_to_S p contents

