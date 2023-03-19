module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Data.Array as Array
import Data.Array((!))


parse rule text = Parsec.parse rule "(source)" text

parseInput :: Parsec.Parsec String () (Integer, Integer)
parseInput = do
  Parsec.string "To continue, please consult the code grid in the manual.  Enter the code at row "
  row <- read <$> Parsec.many1 Parsec.digit
  Parsec.string ", column "  
  col <- read <$> Parsec.many1 Parsec.digit
  Parsec.string "."
  return (row, col)  

fromRight (Right x) = x

cell :: a -> (a -> a) -> Integer -> Integer -> a
cell a f 1 1 = a
cell a f r 1 = f (cell a f 1 (r-1))
cell a f r c = f (cell a f (r+1) (c-1))


main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (row, col) = fromRight $ parse parseInput contents
  print $ cell 20151125 (\x -> (x * 252533) `mod` 33554393) row col
