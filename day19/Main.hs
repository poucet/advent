module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Debug.Trace (trace)
import qualified Data.Map as Map

data Materials = Materials { 
  geode :: Integer ,
  obsidian :: Integer,
  clay :: Integer, 
  ore:: Integer
} deriving (Eq, Ord, Show)

type Robots = Materials

instance Num Materials where
  (Materials a b c d) + (Materials a' b' c' d') = Materials (a+a') (b+b') (c+c') (d+d')
  (Materials a b c d) - (Materials a' b' c' d') = Materials (a-a') (b-b') (c-c') (d-d')
  (Materials a b c d) * (Materials a' b' c' d') = Materials (a*a') (b*b') (c*c') (d*d')
  fromInteger x = Materials x x x x
  abs (Materials a b c d) = Materials (abs a) (abs b) (abs c) (abs d)
  signum (Materials a b c d) = Materials (signum a) (signum b) (signum c) (signum d)

data BluePrint = BluePrint { 
  bluePrintId :: Integer,
  oreRobot :: Materials,
  clayRobot :: Materials,
  obsidianRobot :: Materials,
  geodeRobot :: Materials
} deriving (Eq, Ord, Show)

canBuild :: Materials -> Materials -> Bool
canBuild robot current = 
  (ore robot) <= (ore current) &&
  (clay robot) <= (clay current) &&
  (obsidian robot) <= (obsidian current) &&
  (geode robot) <= (geode current) 

shouldBuild :: (Materials -> Integer) -> BluePrint -> Robots -> Bool
shouldBuild selector blueprint robots = not $ 
  (selector . oreRobot $ blueprint) <= selector robots &&
  (selector . clayRobot $ blueprint) <= selector robots &&
  (selector . obsidianRobot $ blueprint) <= selector robots &&
  (selector . geodeRobot $ blueprint) <= selector robots

parse rule text = Parsec.parse rule "(source)" text

parseInteger :: Parsec.Parsec String () Integer
parseInteger = read <$> Parsec.many1 Parsec.digit

parseMaterial :: Parsec.Parsec String () Materials
parseMaterial = do
  num <- parseInteger
  Parsec.spaces
  materialType <- Parsec.many1 Parsec.lower
  case materialType of
    "ore" -> return $ Materials 0 0 0 num
    "clay" -> return $ Materials 0 0 num 0
    "obsidian" -> return $ Materials 0 num 0 0
    "geode" -> return $ Materials num 0 0 0
    _     -> fail $ "Unknown type: " ++ materialType

parseMaterials :: Parsec.Parsec String () Materials
parseMaterials = do
  material <- parseMaterial
  more <- Parsec.try (do
    Parsec.spaces
    Parsec.string "and"
    Parsec.spaces 
    parseMaterial) <|> (return 0)
  return (material + more)

parseRobot :: String -> Parsec.Parsec String () Materials
parseRobot materialType = do
  Parsec.string "Each"
  Parsec.spaces
  Parsec.string materialType
  Parsec.spaces
  Parsec.string "robot costs"
  Parsec.spaces
  materials <- parseMaterials
  Parsec.char '.'
  Parsec.spaces
  return materials

parseBluePrint :: Parsec.Parsec String () BluePrint
parseBluePrint = do
  Parsec.string "Blueprint"
  Parsec.spaces
  id <- parseInteger
  Parsec.char ':'
  Parsec.spaces
  ore <- parseRobot "ore"
  clay <- parseRobot "clay"
  obsidian <- parseRobot "obsidian"
  geode <- parseRobot "geode"
  return $ BluePrint id ore clay obsidian geode

fromRight :: Either a b -> b
fromRight (Right x) = x

-- Start with a single ore robot
score :: Integer -> BluePrint -> Integer
score turns blueprint = computeResources turns (Materials 0 0 0 1) (Materials 0 0 0 0)
  where computeResources :: Integer -> Robots -> Materials -> Integer
        computeResources 0 robots acc = geode acc
        computeResources n robots acc = 
          let ores'     = robots + acc
              default'  = computeResources (n-1) robots ores'
            in 
              if canBuild (geodeRobot blueprint) acc then
                computeResources (n-1) (robots + (Materials 1 0 0 0)) (ores' - geodeRobot blueprint)
              else if canBuild (obsidianRobot blueprint) acc && shouldBuild obsidian blueprint robots then
                computeResources (n-1) (robots + (Materials 0 1 0 0)) (ores' - obsidianRobot blueprint)
              else foldl max 0 [
                 default',
                if canBuild (clayRobot blueprint) acc && shouldBuild clay blueprint robots then
                  computeResources (n-1) (robots + (Materials 0 0 1 0)) (ores' - clayRobot blueprint)
                else default',
                if canBuild (oreRobot blueprint) acc && shouldBuild ore blueprint robots then
                  computeResources (n-1) (robots + (Materials 0 0 0 1)) (ores' - oreRobot blueprint)
                else default'
              ]
                                            

multiply [] = 1
multiply (x:xs) = x * multiply xs

quality :: BluePrint -> Integer
quality blueprint = (bluePrintId blueprint) * (score 24 blueprint)

problem1 :: [BluePrint] -> IO ()
problem1 blueprints = do
  -- print blueprints
  print $ sum $ map quality $ blueprints

problem2 :: [BluePrint] -> IO ()
problem2 blueprints = do
  print blueprints
  print $ multiply $ map (score 32) $ take 3 blueprints


main = do
  [filename] <- getArgs
  contents <- readFile filename
  let strings = lines contents
  let blueprints = map (fromRight . parse parseBluePrint) strings
  problem1 blueprints
  problem2 blueprints
  