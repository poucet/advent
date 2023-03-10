module Main where
import System.Environment (getArgs)
import Debug.Trace(trace)
import qualified Text.Parsec as Parsec
import Distribution.Simple.Utils (xargs)

parse rule text = Parsec.parse rule "(source)" text 

data Attacker = Attacker {
  hp :: Integer,
  damage :: Integer,
  armor :: Integer
} deriving (Eq, Show, Ord)

data Equipment = Equipment {
  name :: String,
  eqCost :: Integer,
  eqDamage :: Integer,
  eqArmor :: Integer
} deriving (Eq, Show, Ord)

parseBoss :: Parsec.Parsec String () Attacker
parseBoss = do
  Parsec.string "Hit Points:"
  Parsec.spaces
  hp <- read <$> Parsec.many1 Parsec.digit
  Parsec.newline
  Parsec.string "Damage:"
  Parsec.spaces
  damage <- read <$> Parsec.many1 Parsec.digit
  Parsec.newline
  Parsec.string "Armor:"
  Parsec.spaces
  armor <- read <$> Parsec.many1 Parsec.digit
  return $ Attacker hp damage armor

fromRight :: Either a b -> b
fromRight (Right x) = x

hit :: Attacker -> Integer -> Attacker
hit boss dmg = Attacker hp' (damage boss) (armor boss)
  where hp' = hp boss - max 1 (dmg - armor boss)

-- Fights between two attackers, determines whether the first one wins.
fight :: Attacker -> Attacker -> Bool
fight boss player = (hp boss' <= 0) || (hp player' > 0) && fight boss' player'
  where boss'   = hit boss (damage player)
        player' = hit player (damage boss)


makePlayer :: [Equipment] -> Attacker
makePlayer eq = Attacker 100 (sum . map eqDamage $ eq) (sum . map eqArmor $ eq)

weapons :: [Equipment]
weapons = [
    Equipment "Dagger"       8 4 0,
    Equipment "Shortsword"  10 5 0,
    Equipment "Warhammer"   25 6 0,
    Equipment "Longsword"   40 7 0,
    Equipment "Greataxe"    74 8 0
  ]

armors :: [Equipment]
armors = [
    Equipment ""             0 0 0,
    Equipment "Leather"     13 0 1,
    Equipment "Chainmail"   31 0 2,
    Equipment "Splitmail"   53 0 3,
    Equipment "Bandedmail"  75 0 4,
    Equipment "Platemail"  102 0 5  
  ]

rings :: [Equipment]
rings = [
    Equipment "Damage +1"    25 1 0,
    Equipment "Damage +2"    50 2 0,
    Equipment "Damage +3"   100 3 0,
    Equipment "Defense +1"   20 0 1,
    Equipment "Defense +2"   40 0 2,
    Equipment "Defense +3"   80 0 3
  ]

chooseUpTo :: Int -> [a] -> [[a]]
chooseUpTo 0 _       = [[]]
chooseUpTo _ []      = [[]]
chooseUpTo n (o:os)  = chooseUpTo n os ++ map (o:) (chooseUpTo (n-1) os)

pickEquipment :: [[Equipment]]
pickEquipment = do
  weapon <- weapons
  armor <- armors
  fingers <- chooseUpTo 2 rings
  return (weapon:armor:fingers)

main :: IO ()
main = do 
  [filename] <- getArgs
  contents <- readFile filename
  let boss = fromRight . parse parseBoss $ contents
  let possibilities = pickEquipment
  print $ minimum $ map (sum . map eqCost) $ filter (fight boss . makePlayer) possibilities
  --print $ filter ((== 190) . sum . map eqCost) possibilities
  print $ maximum $ map (sum . map eqCost) $ filter (not . fight boss . makePlayer) possibilities