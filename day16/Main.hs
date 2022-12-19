
module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Data.Map as Map
import Data.Map ((!))


parse rule text = Parsec.parse rule "(source)" text

data Valve = Valve {name :: String, rate :: Int, neighbors :: [String]} deriving (Eq, Show)


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

travel :: Map.Map String Valve -> String -> Integer -> Maybe Integer
travel valves at n = do
  let current = valves ! at

  return $ rate current


problem1 :: Map.Map String Valve -> IO ()
problem1 valves = do
  print $ travel valves "AA" 30


main = do
  [filename] <- getArgs
  contents <- readFile filename
  let valves = Map.fromList $ map ((\x -> (name x, x)) . fromRight . parse parseValve) $ lines contents
  problem1 valves