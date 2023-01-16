module Main where
import System.Environment (getArgs)
import Debug.Trace(trace)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import Control.Applicative ((<|>), asum)
import Data.Char
import Data.Set qualified as Set
import Data.List
import Data.Tuple(swap)
import Control.Arrow (ArrowApply(app))

parse rule text = Parsec.parse rule "(source)" text 

data Simple = Electron | Atom Atom deriving (Eq, Ord, Show)
data Token a = Simple Simple | Complex [a] deriving (Eq, Ord, Show)
newtype Molecule = Molecule { unMolecule ::  [Token Molecule] } deriving (Eq, Ord, Show)
type Atom = String

type Replacement = (Simple, [Token Atom]) 

parseElectron :: Parsec.Parsec String () Simple
parseElectron = do
  Parsec.string "e"
  return Electron

parseAtom :: Parsec.Parsec String () Atom
parseAtom = do
  first <- Parsec.upper
  remainder <- Parsec.many Parsec.lower
  case  first:remainder of
    "Y"   -> Parsec.parserFail "Y" 
    "Ar"  -> Parsec.parserFail "Ar" 
    "Rn"  -> Parsec.parserFail "Rn" 
    atom  -> return atom

onlySimple :: Token a -> Bool
onlySimple (Simple _) = True
onlySimple _        = False

isSimpleMolecule :: Molecule -> Bool
isSimpleMolecule (Molecule [Simple x]) = True
isSimpleMolecule _                     = False

parseToken :: Parsec.Parsec String () a -> Parsec.Parsec String () (Token a)
parseToken p = do
  (Complex <$> (Parsec.between (Parsec.string "Rn") (Parsec.string "Ar") $ p `Parsec.sepBy` (Parsec.string "Y"))) <|> Parsec.try (Simple <$> Atom <$> parseAtom)

parseReplacement :: Parsec.Parsec String () Replacement
parseReplacement = do
  left <- parseElectron <|> (Atom <$> parseAtom)
  Parsec.string " => "
  right <- Parsec.many (parseToken parseAtom)
  return (left, right)

parseMolecule :: Parsec.Parsec String () Molecule
parseMolecule = Molecule <$> Parsec.many1 (parseToken parseMolecule)

replace :: [Token Atom] -> [Token Molecule]
replace []              = []
replace (Simple s:xs)   = (Simple s):replace xs
replace (Complex as:xs) = (Complex . (:[]) . Molecule . (map (Simple . Atom)) $ as):replace xs

prepend :: Token Molecule -> Molecule -> Molecule
prepend x (Molecule xs) = Molecule (x:xs)

applyReplacement :: Replacement -> Molecule -> [Molecule]
applyReplacement  _     (Molecule [])  = []
applyReplacement (l, r) (Molecule xs)  = run xs
  where run :: [Token Molecule] -> [Molecule]
        run (Simple x:xs') | x == l     = (Molecule $ replace r ++ xs'):(map (prepend (Simple x)) $ run xs')
                           | otherwise  = (map (prepend (Simple x)) $ run xs')
        run (Complex x:xs')             = map (flip prepend . Molecule $ xs') (map Complex $ runComplex x) ++ (map (prepend (Complex x)) $ run xs')
        run []                          = []
        runComplex :: [Molecule] -> [[Molecule]]
        runComplex []                   = []
        runComplex (m:ms)               = (map (: ms) (applyReplacement (l, r) m)) ++ (map (m:) $ runComplex ms)

fromRight (Right x) = x

step :: [Replacement] -> [Molecule] -> [Molecule]
step replacements molecules = concatMap (\replacement -> concatMap (applyReplacement replacement) molecules) replacements


countTransitions :: Molecule -> Int
countTransitions (Molecule ms) = count ms
  where count [] = 0
        count (Simple _:cs)     = 1 + count cs
        count (Complex rs:cs)   = (count cs) + sum (map countTransitions rs) - length rs + 1

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (r, m) = span ((/=) "") $ lines contents
  let replacements = map (fromRight . parse parseReplacement) r
  let molecule = fromRight . parse parseMolecule .  head . drop 1 $ m
  print . Set.size . Set.fromList $ step replacements [molecule]
  let (easy, hard) = partition (all onlySimple . snd)  replacements
  print $ countTransitions molecule - 1