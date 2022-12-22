module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Parsec
import Control.Applicative ((<|>))

parse rule text = Parsec.parse rule "(source)" text


parseInt :: Parsec.Parsec String () Integer
parseInt = do
  sign <- Parsec.optionMaybe $ Parsec.char '-'
  digits <- read <$> Parsec.many1 Parsec.digit
  case sign of 
    Just '-' -> return $ -digits
    Nothing  -> return digits


parseAttribute :: Parsec.Parsec String () (String, Integer)
parseAttribute = do
  name <- Parsec.many1 Parsec.letter
  Parsec.char ':' >> Parsec.spaces
  quantity <- parseInt
  return (name, quantity)


parseSue :: Parsec.Parsec String () (Integer, [(String, Integer)])
parseSue = do
  "Sue" <- Parsec.many1 Parsec.letter
  Parsec.spaces
  number <- parseInt
  Parsec.char ':' >> Parsec.spaces
  attributes <- parseAttribute `Parsec.sepBy1` (Parsec.char ',' >> Parsec.spaces)
  return (number, attributes)


fromRight (Right x) = x

correctAttr :: (String, Integer) -> Bool
correctAttr ("children", n)     = n == 3
correctAttr ("cats", n)         = n == 7
correctAttr ("samoyeds", n)     = n == 2
correctAttr ("pomeranians", n)  = n == 3
correctAttr ("akitas", n)       = n == 0
correctAttr ("vizslas", n)      = n == 0
correctAttr ("goldfish", n)     = n == 5
correctAttr ("trees", n)        = n == 3
correctAttr ("cars", n)         = n == 2
correctAttr ("perfumes", n)     = n == 1
correctAttr (x, n)              = error $ "Unknown: " ++ x

correctAttr2 :: (String, Integer) -> Bool
correctAttr2 ("children", n)     = n == 3
correctAttr2 ("cats", n)         = n >= 7
correctAttr2 ("samoyeds", n)     = n == 2
correctAttr2 ("pomeranians", n)  = n < 3
correctAttr2 ("akitas", n)       = n == 0
correctAttr2 ("vizslas", n)      = n == 0
correctAttr2 ("goldfish", n)     = n < 5
correctAttr2 ("trees", n)        = n >= 3
correctAttr2 ("cars", n)         = n == 2
correctAttr2 ("perfumes", n)     = n == 1
correctAttr2 (x, n)              = error $ "Unknown: " ++ x

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let sues = map (fromRight . parse parseSue) . lines $ contents
  print $ filter (and . map correctAttr . snd) sues
  print $ filter (and . map correctAttr2 . snd) sues
