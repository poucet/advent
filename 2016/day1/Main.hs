module Main where
import Debug.Trace(trace)
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import qualified Data.Array as Array

parse rule text = Parsec.parse rule "(source)" text

data Rotation = L | R deriving (Eq, Ord, Show)
data Instruction = Instruction {
  rotation :: Rotation,
  distance :: Integer
} deriving (Eq, Ord, Show)

data Heading = N | E | S | W deriving (Eq, Ord, Show)

data State = State {
  heading :: Heading,
  position :: (Integer, Integer)
} deriving (Eq, Ord, Show)

parseRotation :: Char -> Rotation
parseRotation 'R' = R
parseRotation 'L' = L

rotate :: Rotation -> Heading -> Heading
rotate L N = W
rotate L E = N
rotate L S = E
rotate L W = S
rotate R N = E
rotate R E = S
rotate R S = W
rotate R W = N

travel :: Integer -> Heading -> (Integer, Integer) -> (Integer, Integer)
travel d N (x, y) = (x, y - d)
travel d E (x, y) = (x + d, y)
travel d S (x, y) = (x, y + d)
travel d W (x, y) = (x - d, y)

parseInstruction :: Parsec.Parsec String () Instruction
parseInstruction = do 
  rotation <- parseRotation <$> Parsec.oneOf "RL"
  distance <- read <$> Parsec.many1 Parsec.digit
  return $ Instruction rotation distance

parseInstructions :: Parsec.Parsec String () [Instruction]
parseInstructions = parseInstruction `Parsec.sepBy` (Parsec.string ", ")

fromRight (Right x) = x
fromMaybe (Just x) = x

apply :: Instruction -> State -> State
apply ins state = State heading' position'
  where heading'  = rotate (rotation ins) (heading state)
        position' = travel (distance ins) heading' (position state)

distToOrigin :: (Integer, Integer) -> Integer
distToOrigin pos = abs (fst pos) + abs (snd pos)

execute :: [Instruction] -> [State]
execute ins = scanl (flip apply) (State N (0, 0)) ins

findPos :: [(Integer, Integer)] -> Maybe (Integer, Integer)
findPos []      = Nothing
findPos [x]     = Nothing
findPos (x:xs)  = if x `elem` xs then Just x else findPos xs

walk :: [State] -> [(Integer, Integer)]
walk []         = []
walk [s]        = [position s]
walk (s1:s2:ss) = walk' (position s1) (position s2) (heading s2) ++ walk (s2:ss)
  where walk' s1 s2 N | (fst s1 == fst s2) = [(fst s1, y) | y <- reverse [(snd s2 + 1) .. (snd s1)]]
        walk' s1 s2 E | (snd s1 == snd s2) = [(x, snd s1) | x <- [fst s1 .. (fst s2 - 1)]]
        walk' s1 s2 S | (fst s1 == fst s2) = [(fst s1, y) | y <- [snd s1 .. (snd s2 - 1)]]
        walk' s1 s2 W | (snd s1 == snd s2) = [(x, snd s1) | x <- reverse [(fst s2 + 1) .. (fst s1)]]

main = do 
  [filename] <- getArgs
  contents <- readFile filename
  let instructions = fromRight $ parse parseInstructions contents
  let states = execute instructions
  print . distToOrigin . position . last $ states
  print . distToOrigin . fromMaybe . findPos . walk $ states
cd ..