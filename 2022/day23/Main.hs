module Main where
import Data.Array as Array
import Debug.Trace(trace)
import Data.Ix(Ix, range, inRange)
import System.Environment (getArgs)
import Control.Monad(ap)
import Data.Tuple(swap)
import Data.List(tails)

type Grid i = Array.Array i Bool
type Rule i = [((i) -> [(i)], (i) -> (i))]

isAlive '#' = True
isAlive '.' = False

north :: (Enum a, Enum b) => (a, b) -> (a, b)
north (i, j)  = (i, pred j)
south :: (Enum a, Enum b) => (a, b) -> (a, b)
south (i, j)  = (i, succ j)
west :: (Enum a, Enum b) => (a, b) -> (a, b)
west (i, j)   = (pred i, j)
east :: (Enum a, Enum b) => (a, b) -> (a, b)
east (i, j)   = (succ i, j)

check_n :: (Enum a, Enum b) => (a, b) -> [(a, b)]
check_n = ap [north . west, north,  north . east] . return
check_s :: (Enum a, Enum b) => (a, b) -> [(a, b)]
check_s = ap [south . west, south, south . east] . return
check_w :: (Enum a, Enum b) => (a, b) -> [(a, b)]
check_w = ap [north . west, west, south . west] . return
check_e :: (Enum a, Enum b) => (a, b) -> [(a, b)]
check_e = ap [north . east, east, south . east] . return

neighbors :: (Enum a, Enum b) => (a, b) -> [(a, b)]
neighbors = ap [north . west, north, north . east,
                west        ,        east        ,
                south . west, south, south . east] . return

check :: (Ix i) => Grid i -> i -> Bool
check grid i = inRange (Array.bounds grid) i && grid Array.! i

rules :: (Ix i, Enum i, Ix j, Enum j) => [((i,j) -> [(i,j)], (i,j) -> (i,j))]
rules = [(check_n, north),
         (check_s, south),
         (check_w, west),
         (check_e, east)]

propose :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> Rule (i, j) -> (i, j) -> [(i, j)]
propose grid rules p = case any (check grid) $ neighbors p of
  False -> []
  True  -> findfirst rules
  where findfirst []      = []
        findfirst (x:xs)  = if not $ any (check grid) $ fst x $ p then [snd x p]
                            else findfirst xs

toString :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> String
toString grid =
  unlines [[if c then '#' else '.' | i <- [mini..maxi],let c = grid Array.! (i, j) ] | j <- [minj..maxj]]
  where ((mini, minj), (maxi, maxj)) = Array.bounds grid

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

step :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> Rule (i, j) -> Grid (i, j)
step grid rules = Array.accum (flip const) grid' $ validProposals
  where grid'          = grow grid
        bounds'        = Array.bounds grid'
        proposalGrid   = Array.accum (flip (:)) (Array.array bounds' [(i, []) | i <- range bounds' ]) propositions
        propositions   = [(j, i) | i <- range bounds', grid' Array.! i, j <- propose grid rules i]
        validProposals = concatMap (\((i, j), [(a, b)]) -> [((i,j), True), ((a,b), False)]) $ filter ((== 1) . length . snd) $ Array.assocs proposalGrid

grow :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> Grid (i, j)
grow grid = Array.array bounds' [(i, inRange bounds i && grid Array.! i) | i <- range bounds' ]
  where bounds   = Array.bounds grid
        bounds'  = ((pred $ fst $ fst bounds, pred $ snd $ fst bounds),
                    (succ $ fst $ snd bounds, succ $ snd $ snd bounds))

crop :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> Grid (i, j)
crop grid = Array.array bounds' [(i, grid Array.! i) | i <- range bounds' ]
  where bounds   = Array.bounds grid
        bounds'  = ((succ $ fst $ fst bounds, succ $ snd $ fst bounds),
                    (pred $ fst $ snd bounds, pred $ snd $ snd bounds))

computeSurface :: (Ix i, Ix j, Enum i, Enum j) => Grid (i, j) -> Int
computeSurface grid = (maxx - minx + 1) * (maxy - miny + 1) - total
  where minx = minimum [fromEnum x | (x,y) <- dots]
        maxx = maximum [fromEnum x | (x,y) <- dots]
        miny = minimum [fromEnum y | (x,y) <- dots]
        maxy = maximum [fromEnum y | (x,y) <- dots]
        dots = map fst $ filter (snd) $ Array.assocs grid
        total = length dots

gridEqual :: (Ix i, Enum i, Ix j, Enum j) => Grid (i, j) -> Grid (i, j) -> Bool
gridEqual x y = x == crop y

countEqual :: [Grid (Int, Int)] -> Int
countEqual grids = run grids 1
  where run []        n = 0
        run [x]       n = 0
        run (x:y:xs)  n = trace (show n ++ "\n") (if gridEqual x y then n else run (y:xs) (n+1))

parse :: String -> IO (Grid (Int, Int))
parse filename = do
  contents <- readFile filename
  let rows = lines contents
  let (height, width) = (length rows, length $ head rows)
  let grid = Array.array ((1, 1), (width, height)) [((i, j), isAlive c) | (j, row) <- zip [1..height] rows, (i, c) <- zip [1..width] row]
  return grid

main = do
  [filename] <- getArgs
  grid <- parse filename
  let grids = scanl step grid (map (take 4) $ tails $ cycle rules)
  mapM_ (print . computeSurface) . take 1 . drop 10 $ grids
  print $ countEqual grids
