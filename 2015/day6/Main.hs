module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c l = go l []
  where go [] []                  = []
        go [] acc                 = [reverse  acc]
        go (x:xs) acc | x == c    = (reverse acc):(go xs [])
        go (x:xs) acc | otherwise = (go xs (x:acc))

type Coordinate = (Integer, Integer)
data Action = Toggle | On | Off deriving (Eq, Show)
data Square = Square {from :: Coordinate, to :: Coordinate} deriving (Eq, Show)

data Instruction = Instruction { action :: Action, square :: Square } deriving (Eq, Show)

contains :: Square -> Coordinate -> Bool
contains (Square f t) c = between (fst f) (fst t) (fst c) && between (snd f) (snd t) (snd c) 
  where between x y z = x <= z && z <= y

parse :: (Parsec String () a) -> String -> a
parse rule text = Parsec.parse rule "(source)" text

parsePair :: Parsec.Parsec String () Coordinate
parsePair = do 
  x <- read <$> Parsec.many1 Parsec.digit 
  Parsec.char ','
  y <- read <$> Parsec.many1 Parsec.digit 
  return (x, y)

parseAction :: Parsec.Parsec String () Action
parseAction = parseToggle <|> parseOn <|> parseOff
  where parseToggle = Parsec.try (Parsec.string "toggle") >> return Toggle
        parseOff = Parsec.try (Parsec.string "turn off") >> return Off
        parseOn = Parsec.try (Parsec.string "turn on") >> return On

parseInstruction :: Parsec.Parsec String () Instruction
parseInstruction = do
  action <- parseAction
  Parsec.spaces
  from <- parsePair
  Parsec.spaces
  Parsec.string "through"
  Parsec.spaces
  to <- parsePair
  return $ Instruction action $ Square from to

fromRight :: Either a b -> b
fromRight (Right x) = x

isOn :: [Instruction] -> Coordinate -> Bool
isOn [] p                         = False
isOn (i:xs)    p  = 
  case action i of 
    Off -> (not $ contains s p) && isOn xs p
    On ->  (contains s p) || isOn xs p
    Toggle -> if contains s p then not $ isOn xs p else isOn xs p
  where s = square i

lightLevel :: [Instruction] -> Coordinate -> Integer
lightLevel []      p = 0
lightLevel (i:is)  p = 
  if contains (square i) p then
    case (action i) of 
      Off -> 
        let remaining = (lightLevel is p) in
        if remaining <= 0 then remaining else remaining - 1
      On -> 1 + (lightLevel is p)
      Toggle -> 2 + (lightLevel is p)
  else (lightLevel is p)

problem1 :: [Instruction] -> IO ()
problem1 instructions = 
  print $ length [(i, j) | i <- [0..999], j <- [0..999], isOn (reverse instructions)  (i,j)]

problem2 :: [Instruction] -> IO ()
problem2 instructions = 
  print $ sum $ map (lightLevel (reverse instructions)) [(i, j) | i <- [0..999], j <- [0..999]]

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let strings = splitOn '\n' contents
  let instructions = map fromRight $ map (parse parseInstruction) strings
  problem1 instructions
  problem2 instructions
  
  
  