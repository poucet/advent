module Main where
import System.Environment (getArgs)
import Data.Array qualified as Array

alive '#' = True
alive '.' = False


neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i-1, j-1), (i,   j-1), (i+1, j-1),
                    (i-1,   j),             (i+1, j),
                    (i-1, j+1), (i,   j+1), (i+1, j+1)]

check :: Array.Array ((Int, Int)) Bool -> (Int, Int) -> Bool
check grid (i, j) | (mini <= i && i <= maxi && minj <= j && j <= maxj) = grid Array.! (i, j)
                  | otherwise                                          = False
  where ((mini, minj), (maxi, maxj)) = Array.bounds grid

step :: Array.Array ((Int, Int)) Bool -> Array.Array ((Int, Int)) Bool 
step grid = grid'
  where grid' = Array.array ((mini, minj), (maxi, maxj)) [((i,j), live (i,j)) | i <- [mini..maxi], j <- [minj..maxj]]
        ((mini, minj), (maxi, maxj)) = Array.bounds grid
        live c = let ns = length $ filter (check grid) $ neighbors c in
          if grid Array.! c then ns >= 2 && ns <= 3 else ns == 3

lightCorners :: Array.Array ((Int, Int)) Bool -> Array.Array ((Int, Int)) Bool
lightCorners grid = grid Array.// [((i, j), True) | i <- [1,100], j <- [1,100]]

main = do
  [filename] <- getArgs
  contents <- filter ((/=) "") . lines <$> readFile filename
  let (width, height) = (length $ head $ contents, length contents)
  let grid  = Array.array ((1, 1), (width, height)) [((i, j), alive c) | (row, j) <- zip contents [1..], (c, i) <- zip row [1..]] 
  print $ length $ filter id $ Array.elems $ head $ drop 100 $ iterate step grid
  print $ length $ filter id $ Array.elems $ head $ drop 100 $ iterate (lightCorners . step) $ lightCorners grid
