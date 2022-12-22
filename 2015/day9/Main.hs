
module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List (permutations)
import Debug.Trace (trace)

parse rule text = Parsec.parse rule "(source)" text

parseLine :: Parsec.Parsec String () [((String, String), Integer)]
parseLine = do
  city1 <- Parsec.many1 Parsec.letter
  Parsec.spaces
  Parsec.string "to"
  Parsec.spaces
  city2 <- Parsec.many1 Parsec.letter
  Parsec.spaces
  Parsec.char '='
  Parsec.spaces
  distance <- read <$> Parsec.many1 Parsec.digit
  return [((city1, city2), distance), ((city2, city1), distance)]

distance :: Map.Map (String, String) Integer -> [String] -> Integer
distance distances l = run 0 l
  where run n []        = n
        run n [x]       = n
        run n (x:y:xs)  = run (n + distances Map.! (x, y)) (y:xs)
 
problem1 :: Map.Map (String, String) Integer -> IO ()
problem1 distances = do
  print $ minimum $ map (distance distances) $ permutations cities
  where cities = Set.toList $ Set.fromList $ map fst $ Map.keys distances
  
problem2 :: Map.Map (String, String) Integer -> IO ()
problem2 distances = do
  print $ maximum $ map (distance distances) $ permutations cities
  where cities = Set.toList $ Set.fromList $ map fst $ Map.keys distances

fromRight (Right x) = x

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let distances = Map.fromList $ concatMap (fromRight . parse parseLine) $ lines contents
  problem1 distances
  problem2 distances