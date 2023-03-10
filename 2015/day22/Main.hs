module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import AStar (AStep(..), aStarOn)

parse rule text = Parsec.parse rule "(source)" text 

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Eq, Show)

cost :: Spell -> Integer
cost MagicMissile = 53
cost Drain        = 73
cost Shield       = 113
cost Poison       = 173
cost Recharge     = 229

damage :: Spell -> Integer
damage MagicMissile = 4
damage Drain        = 2
damage _            = 0

heal :: Spell -> Integer
heal Drain = 2
heal _     = 0

data Timers = Timers {
  shield :: Integer,
  poison :: Integer,
  recharge :: Integer
} deriving (Eq, Ord, Show)

data GameState = GameState {
  mana :: Integer,
  playerHp :: Integer,
  bossHp :: Integer,
  bossDmg :: Integer,
  timers :: Timers
} deriving (Eq, Ord, Show)

doAction :: (GameState -> GameState) -> (GameState -> [GameState])
doAction f s
  | playerHp s <= 0 = []
  | bossHp s <= 0   = [s]
  | otherwise       = [f s]

chooseSpell :: GameState -> [Spell]
chooseSpell s = filter (\spell -> cost spell <= mana s) 
  $   [ Shield | shield (timers s) == 0] 
  ++  [ Drain  | poison (timers s) == 0]
  ++  [ Recharge | recharge (timers s) == 0]
  ++  [ MagicMissile, Drain]

castSpell :: Spell -> GameState -> GameState
castSpell spell s = s {
    mana = mana s - cost spell,
    timers = Timers {
      shield   = if spell == Shield then 6 else shield t,
      poison   = if spell == Poison then 6 else poison t,
      recharge = if spell == Recharge then 5 else recharge t
    },
    playerHp = playerHp s + heal spell,
    bossHp   = playerHp s - damage spell
  }
  where t = timers s

bossAttack :: GameState -> GameState
bossAttack s = s { playerHp = playerHp s - dmg }
  where dmg   = max 1 (bossDmg s - armor)
        armor = if (shield . timers $ s) > 0 then 7 else 0

decrease :: Integer -> Integer
decrease x = max 0 (x - 1)

tickTimers :: GameState -> GameState
tickTimers s = s {
    mana    = mana s + if (recharge . timers $ s) > 0 then 101 else 0,
    bossHp  = bossHp s - if (poison . timers $ s) > 0 then 3 else 0,
    timers = Timers {
      shield = decrease . shield . timers $ s,
      poison   = decrease . poison . timers $ s,
      recharge = decrease . recharge . timers $ s
    }
  }

battleRound :: GameState -> [AStep GameState Integer]
battleRound s = do 
  s1 <- doAction tickTimers s
  spell <- chooseSpell s1
  s2 <- doAction (castSpell spell) s1
  s3 <- doAction tickTimers s2
  s4 <- doAction bossAttack s3
  return $ AStep s4 (cost spell) 0


-- Returns the hp and damage of the boss
parseBoss :: Parsec.Parsec String () (Integer, Integer) 
parseBoss = do
  Parsec.string "Hit Points:"
  Parsec.spaces
  hp <- read <$> Parsec.many1 Parsec.digit
  Parsec.newline
  Parsec.string "Damage:"
  Parsec.spaces
  dmg <- read <$> Parsec.many1 Parsec.digit
  return (hp, dmg)

fromRight (Right x) = x

battle :: GameState -> [Integer]
battle s = take 10 $ [ cost | (s', cost) <- aStarOn id battleRound [s], bossHp s' <= 0]

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let boss = fromRight $ parse parseBoss contents
  let game = GameState {
    mana = 500,
    playerHp = 50,
    bossHp = fst boss,
    bossDmg = snd boss,
    timers = Timers 0 0 0
  }
  print $ battle game