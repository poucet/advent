module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<|>))

parse rule text = Parsec.parse rule "(source)" text

data Contents = Contents {
  capacity :: Integer, 
  durability :: Integer, 
  flavor :: Integer,
  texture :: Integer, 
  calories :: Integer
} deriving (Eq, Show)

applyBinOp :: (Integer -> Integer -> Integer) -> Contents -> Contents -> Contents
applyBinOp op a b = Contents (capacity a `op` capacity b) (durability a `op` durability b) (flavor a `op` flavor b) (texture a `op` texture b) (calories a `op` calories b)

applyOp :: (Integer -> Integer) -> Contents -> Contents
applyOp op (Contents c d f t cal) = Contents (op c) (op d) (op f) (op t) (op cal)


instance Num Contents where
  a + b         = applyBinOp (+) a b
  a * b         = applyBinOp (*) a b
  a - b         = applyBinOp (-) a b
  abs a         = applyOp abs a
  signum a      = applyOp signum a
  fromInteger n = Contents n n n n n


data Ingredient = Ingredient { 
  name :: String, 
  contains :: Contents
} deriving (Eq, Show)

parseInt :: Parsec.Parsec String () Integer
parseInt = do
  sign <- Parsec.optionMaybe $ Parsec.char '-'
  digits <- read <$> Parsec.many1 Parsec.digit
  case sign of 
    Just '-' -> return $ -digits
    Nothing  -> return digits


parseIngredient :: Parsec.Parsec String () Ingredient
parseIngredient = do
  name <- Parsec.many1 Parsec.letter
  Parsec.char ':'
  Parsec.spaces
  Parsec.string "capacity"
  Parsec.spaces
  capacity <- parseInt
  Parsec.char ','
  Parsec.spaces
  Parsec.string "durability"
  Parsec.spaces
  durability <- parseInt
  Parsec.char ','
  Parsec.spaces
  Parsec.string "flavor"
  Parsec.spaces
  flavor <- parseInt
  Parsec.char ','
  Parsec.spaces
  Parsec.string "texture"
  Parsec.spaces
  texture <- parseInt
  Parsec.char ','
  Parsec.spaces
  Parsec.string "calories"
  Parsec.spaces
  calories <- parseInt
  return $ Ingredient name $ Contents capacity durability flavor texture calories



fromRight (Right x) = x

multiply l = foldl1 (*) l

combine :: [Ingredient] -> [Integer] -> Contents
combine [] _          = Contents 0 0 0 0 0
combine _ []          = Contents 0 0 0 0 0
combine (x:xs) (y:ys) = (contains x * fromInteger y) + (combine xs ys)

score :: Contents -> Integer
score (Contents c d f t _) = multiply $ map (max 0) [c,d,f,t]

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let [x,y,z,w] = map (fromRight .parse parseIngredient) . lines $ contents
  let teaspoons = [[(contains x) * fromInteger a, 
                    (contains y) * fromInteger b, 
                    (contains z) * fromInteger c, 
                    (contains w) * fromInteger d]
                   | a <- [0..100], 
                     b <- [0..100], 
                     c <- [0..100], 
                     d <- [0..100], 
                     a + b + c + d == 100
                    ]
  print $ maximum $ map (score . sum) $ teaspoons
  print $ maximum $ map score $ filter ((== 500) . calories) $ map sum $ teaspoons
