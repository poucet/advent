module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<|>))
import Data.Set qualified as Set
import Data.Tuple(swap)

parse rule text = Parsec.parse rule "(source)" text


parseReplacement :: Parsec.Parsec String () (String, String)
parseReplacement = do
  left <- Parsec.many Parsec.letter
  Parsec.string " => "
  right <- Parsec.many Parsec.letter
  return (left, right)

applyReplacement :: String -> (String, String) -> [String]
applyReplacement [] _       = []
applyReplacement xs (l, r)  =
  case take ll xs of
    l' | l == l'   -> [r ++ (drop ll xs)] ++ (map ((head xs):) $ applyReplacement (tail xs) (l, r))
       | otherwise -> map ((head xs):) $ applyReplacement (tail xs) (l, r)
  where ll = length l

fromRight (Right x) = x


step :: [(String, String)] -> Set.Set String -> Set.Set String
step replacements molecules = Set.unions $ map (\molecule -> Set.unions $ map (Set.fromList . applyReplacement molecule) replacements) $ Set.toList molecules



main = do
  [filename] <- getArgs
  contents <- readFile filename
  let replacements = map (fromRight . parse parseReplacement) $ takeWhile ((/=) "") $ lines contents
  let molecule = head $ drop 1 $ dropWhile ((/=) "") $ lines contents
  print $ Set.size $ Set.fromList $ concatMap (applyReplacement molecule) replacements
  print $ length $ takeWhile (not . Set.member "e") $ iterate (step (map swap replacements)) (Set.singleton molecule)