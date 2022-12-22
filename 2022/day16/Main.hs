{-# Language BlockArguments #-}
module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import Data.Set qualified as Set
import Data.Map ((!))
import Data.List (foldl')
import Data.Maybe (maybeToList)
import Debug.Trace(trace)

-- Based on https://github.com/glguy/advent/blob/main/solutions/src/2022/16.hs

parse rule text = Parsec.parse rule "(source)" text

data Valve = Valve {name :: String, rate :: Integer, neighbors :: [String]} deriving (Eq, Show)

parseNeighors :: Parsec.Parsec String () [String]
parseNeighors = do
  name <- Parsec.many1 Parsec.upper
  more <- (Parsec.try $ Parsec.string "," >> Parsec.spaces >> parseNeighors ) <|> return []
  return $ name:more

parseValve :: Parsec.Parsec String () Valve
parseValve = do 
  Parsec.string "Valve"
  Parsec.spaces
  name <- Parsec.many1 Parsec.upper
  Parsec.spaces
  Parsec.string "has flow rate="
  rate <- read <$> Parsec.many1 Parsec.digit
  (Parsec.try $ Parsec.string "; tunnels lead to valves") <|> (Parsec.try $ Parsec.string "; tunnel leads to valve") 
  Parsec.spaces
  neighbors <- parseNeighors
  return $ Valve name rate neighbors

fromRight (Right x) = x

floydWarshall :: (Eq a, Ord a) => [a] -> Map.Map (a, a) Integer -> Map.Map (a, a) Integer
floydWarshall keys = 
  forEach \k -> 
    forEach \i -> 
      forEach \j dists ->
        case (Map.lookup (i,k) dists, Map.lookup (k,j) dists) of
          (Just d1, Just d2) -> StrictMap.insertWith min (i,j) (d1+d2) dists
          _                  -> dists
  where forEach f d = foldl' (flip f) d keys


solve :: Map.Map String [(String, Integer, Integer)] -> Integer -> String -> Map.Map (Set.Set String) Integer
solve graph n start = StrictMap.fromListWith max (go [(n, start, Set.empty, 0)])
  where go xs = [(open, flow) | (_, _, open, flow) <- xs] ++ concatMap (go . step) xs
        step (t, current, openedValves, flow) =
          [(t', next, Set.insert next openedValves, flow + t' * valve)
              | (next, cost, valve) <- graph Map.! current
              , not (Set.member next openedValves)
              , let t' = t - cost
              , t' > 0]

problem1 :: Map.Map String [(String, Integer, Integer)] -> IO ()
problem1 graph =
  print $ maximum $ solve graph 30 "AA"

problem2 :: Map.Map String [(String, Integer, Integer)] -> IO ()
problem2 graph =
  print $ maximum [ v1 + v2 | (open1, v1) <- allSolutions, (open2, v2) <- allSolutions, Set.disjoint open1 open2]
  where allSolutions = Map.assocs $ solve graph 26 "AA"


main = do
  [filename] <- getArgs
  contents <- readFile filename
  let valves = map (fromRight . parse parseValve) $ lines contents
  let distances1 = Map.fromList [((k,v), 1) | (Valve k r vs) <- valves, v <- vs]
  let distances = floydWarshall (map name valves) distances1
  let flowRates = Map.fromList [(name valve, rate valve) | valve <- valves, rate valve > 0]
  let graph = Map.fromListWith (++) 
                [(src, [(dst,cost+1, flow)])
                  | ((src,dst), cost) <- Map.assocs distances
                  , src == "AA" || Map.member src flowRates, src /= dst
                  , flow <- maybeToList (Map.lookup dst flowRates)]
  
  problem1 graph
  problem2 graph

