module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List (permutations)

parse rule text = Parsec.parse rule "(source)" text

parseHappiness :: Parsec.Parsec String () ((String, String), Integer)
parseHappiness = do
  name <- Parsec.many1 Parsec.letter
  Parsec.spaces
  Parsec.string "would"
  Parsec.spaces
  bit <- (Parsec.string "gain" <|> Parsec.string "lose")
  Parsec.spaces
  amount <- read <$> Parsec.many1 Parsec.digit
  Parsec.spaces
  Parsec.string "happiness units by sitting next to"
  Parsec.spaces
  neighbor <- Parsec.many1 Parsec.letter
  Parsec.char '.'
  case bit of
    "gain" -> return ((name, neighbor), amount)
    "lose" -> return ((name, neighbor), -amount)

fromRight (Right x) = x

score ::Map.Map (String, String) Integer -> [String] -> Integer
score potential table = score' $ table ++ [head table]
  where score' []       = 0
        score' [x]      = 0
        score' (x:y:xs) = (potential Map.! (x, y)) + (potential Map.! (y, x)) + score' (y:xs)

problem1 :: Map.Map (String, String) Integer -> [String] -> IO ()
problem1 potential people = do
  print $ maximum $ map (score potential) $ permutations people

problem2 :: Map.Map (String, String) Integer -> [String] -> IO ()
problem2 potential people = do
  print $ maximum $ map (score potential') $ permutations $ people ++ ["me"]
  where potential' = potential `Map.union` (Map.fromList $ concat [[((x, "me"), 0), (("me", x), 0)] | x <- people])

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let potential = Map.fromList $ map (fromRight . parse parseHappiness) $ lines contents
  let people = Set.toList $ Set.fromList $ map fst $ Map.keys potential
  problem1 potential people
  problem2 potential people

