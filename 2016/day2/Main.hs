module Main where
import Debug.Trace(trace)
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import qualified Data.Array as Array
import qualified Distribution.Compat.CharParsing as Parse


parse rule text = Parsec.parse rule "(source)" text
fromRight (Right x) = x
  
data Step = L | R | U | D deriving (Eq, Show, Read, Ord)

move :: (Eq a, Num a) => Step -> a -> a
move U 1 = 1
move U 2 = 2
move U 3 = 3
move U x = x - 3
move L 1 = 1
move L 4 = 4
move L 7 = 7
move L x = x - 1
move D 7 = 7
move D 8 = 8
move D 9 = 9
move D x = x + 3
move R 3 = 3
move R 6 = 6
move R 9 = 9
move R x = x + 1

parseStep :: Parsec.Parsec String () Step
parseStep = do 
  c <- Parsec.oneOf "LRUD"
  return $ case c of 
    'L' -> L
    'U' -> U
    'R' -> R
    'D' -> D

parseLine :: Parsec.Parsec String () [Step]
parseLine = Parsec.many1 parseStep

parseFile :: Parsec.Parsec String () [[Step]]
parseFile = parseLine `Parsec.sepBy` Parse.newline

execute :: [[Step]] -> [Integer]
execute ss = execute' 5 ss
  where execute' :: Integer -> [[Step]] -> [Integer]
        execute' n []       = []
        execute' n (s:ss)   = n':execute' n' ss
          where n' = foldl (flip move) n s

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let steps = fromRight $ parse parseFile contents
  print $ execute $ steps
  
 