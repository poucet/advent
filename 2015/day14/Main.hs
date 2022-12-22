module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Function (on)
import Data.List (maximumBy, transpose)

parse rule text = Parsec.parse rule "(source)" text

parseReindeer :: Parsec.Parsec String () (String, Integer, Integer, Integer)
parseReindeer = do
  name <- Parsec.many1 Parsec.letter
  Parsec.spaces
  Parsec.string "can fly"
  Parsec.spaces
  speed <- read <$> Parsec.many1 Parsec.digit
  Parsec.spaces 
  Parsec.string "km/s for"
  Parsec.spaces
  duration <- read <$> Parsec.many1 Parsec.digit
  Parsec.spaces
  Parsec.string "seconds, but then must rest for"
  Parsec.spaces
  pause <- read <$> Parsec.many1 Parsec.digit
  Parsec.spaces
  Parsec.string "seconds."
  return (name, speed, duration, pause)

fromRight (Right x) = x

score :: Integer -> (String, Integer, Integer, Integer) -> Integer
score n (name, speed, duration, pause) = ((n `div` (duration + pause)) * speed * duration) + (min (n `mod` (duration + pause)) duration) * speed


race :: (String, Integer, Integer, Integer) -> [Integer]
race (name, speed, duration, pause) = scanl1 (+) $ cycle $ replicate (fromEnum duration) speed ++ replicate (fromEnum pause) 0

problem1 :: [(String, Integer, Integer, Integer)] -> Integer -> Integer
problem1 reindeer n = maximum $ map (score n) reindeer

problem2 :: [(String, Integer, Integer, Integer)] -> Integer -> [(String, Integer)]
problem2 reindeer n = Map.toList $ Map.fromListWith (+) [(n, 1) | n <- concatMap lead [1..n]]
  where lead i = let scores = map (\d@(n,_,_,_) -> (n, score i d)) reindeer
                     maxScore = maximumBy (compare `on` snd) scores in
                       [n | (n, d) <- scores, d == (snd maxScore)]


main = do
  [filename, num] <- getArgs
  contents <- readFile filename
  let reindeer = map (fromRight . parse parseReindeer) . lines $ contents
  let fullRace = map (\(n, r) -> (n, take (read num) r)) $ map (\d@(name, _, _, _) -> (name, race d)) reindeer
  print $ problem1 reindeer (read num)
  print $ problem2 reindeer (read num)
