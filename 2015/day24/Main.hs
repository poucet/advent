module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Data.Array as Array
import Data.Maybe (listToMaybe)
import Data.List (sortOn)
import Control.Arrow ((&&&))


parse rule text = Parsec.parse rule "(source)" text

parseInt :: Parsec.Parsec String () Integer
parseInt = do
  read <$> Parsec.many1 Parsec.digit

parseInput :: Parsec.Parsec String () [Integer]
parseInput = Parsec.sepBy parseInt Parsec.newline

fromRight :: Either a b -> b
fromRight (Right x) = x

-- Selects up to the goal size from nums into splits 
-- Where the first component is the numbers that add up to the given goal
-- and the second set of numbers are the remaining numbers.
select :: Integer -> [Integer] -> [([Integer], [Integer])]
select goal nums = run nums [] []
  where run nums    ps qs | sum ps == goal = [(ps, nums ++ qs)]
        run nums    ps qs | sum ps > goal  = []
        run []      ps qs                  = []
        run (n:ns)  ps qs                  = run ns (n:ps) qs ++ run ns ps (n:qs)

solve :: Integer -> [Integer] -> [[Integer]]
solve n nums = do
  (p1, nums')   <- sortOn (length . fst &&& product . fst) $ select goal nums
  () <- solveAgain (n-1) nums'
  return p1
  where goal    = sum nums `div` n
        solveAgain 0 []     = [()]
        solveAgain 0 _      = []
        solveAgain i []     = []
        solveAgain i nums'  = do
           (p2, nums'')  <- select goal nums'
           solveAgain (i-1) nums''

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let nums = fromRight $ parse parseInput contents
  print $ product $ head $ solve 3 nums
  print $ product $ head $ solve 4 nums