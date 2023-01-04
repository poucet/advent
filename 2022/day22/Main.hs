module Main where
import System.Environment (getArgs)
import Debug.Trace(trace)
import Data.Array qualified as Array
import Debug.Trace(trace)

data Command = Move Int | CW | CCW deriving (Eq, Show)
data Position = Pos {pos :: (Int, Int), dir :: Direction} deriving (Eq, Show)
data Direction = U | D | L | R deriving (Eq, Show)

isEmpty :: Array.Array (Int, Int) Char -> (Int, Int) -> Bool
isEmpty map pos = (map Array.! pos) == ' '

isBlocked :: Array.Array (Int, Int) Char -> (Int, Int) -> Bool
isBlocked map pos = (map Array.! pos) == '#'

cw :: Position -> Position
cw p = Pos (pos p) (cw' $ dir p)
  where cw' U = R
        cw' D = L
        cw' L = U
        cw' R = D

ccw :: Position -> Position
ccw p = Pos (pos p) (ccw' $ dir p)
  where ccw' U = L
        ccw' D = R
        ccw' L = D
        ccw' R = U

step :: Position -> Position
step (Pos (i, j) U) = Pos (i, j - 1) U
step (Pos (i, j) D) = Pos (i, j + 1) D 
step (Pos (i, j) R) = Pos (i + 1, j) R
step (Pos (i, j) L) = Pos (i - 1, j) L


-- Assumes a map of a cube with sides of length 50 shaped like
--    _4_[1][2]_5_
--    _4_[3]_2_
--    [4][5]_2__1_
--    [6]__   _1_
--    _ _
cubeFace :: (Int, Int) -> Int
cubeFace (0, 0) = error "Invalid coordinate"
cubeFace (0, j) = cubeFace(151, j)
cubeFace (i, 0) = cubeFace (i, 201)
cubeFace (i, j) = 
  case ((i - 1) `div` 50, (j - 1) `div` 50) of
    (1, 0) -> 1
    (2, 0) -> 2
    (1, 1) -> 3
    (0, 2) -> 4
    (1, 2) -> 5
    (0, 3) -> 6
    _      -> -1

walk1 :: Array.Array (Int, Int) Char -> Position -> Position
walk1 map p = if isBlocked map (pos p') then p else p'
  where p' = head $ dropWhile (isEmpty map . pos) $ drop 1 $ iterate (wrap . step) p
        wrap (Pos (i, j) d) = Pos (bound mini maxi i, bound minj maxj j) d
        bound min max x = min + ((x - min) `mod` (max + 1 - min))
        ((mini, minj), (maxi, maxj)) = Array.bounds map

walk2 :: Array.Array (Int, Int) Char -> Position -> Position
walk2 map p = if isBlocked map (pos p'') then p else if isEmpty map (pos p'') then error $ "Invalid walk" ++ debug p'' else p''
  where (i, j) = pos p
        p'  = step p
        p'' = case (cubeFace $ pos p, cubeFace $ pos p', dir p) of
                  (x, y, _) | x == y  -> p'
                  (1, _, R)           -> p'
                  (1, _, D)           -> p'
                  (1, _, L)           -> Pos (1, 151 - j) R
                  (1, _, U)           -> Pos (1, i + 100) R
                  (2, _, L)           -> p'
                  (2, _, D)           -> Pos (100, i - 50) L
                  (2, _, R)           -> Pos (100, 151 - j) L
                  (2, _, U)           -> Pos (i - 100, 200) U
                  (3, _, U)           -> p'
                  (3, _, D)           -> p'
                  (3, _, L)           -> Pos (j - 50, 101) D
                  (3, _, R)           -> Pos (j + 50, 50) U
                  (4, _, R)           -> p'
                  (4, _, D)           -> p'
                  (4, _, U)           -> Pos (51, 50 + i) R
                  (4, _, L)           -> Pos (51, 151 - j) R
                  (5, _, U)           -> p'
                  (5, _, L)           -> p'
                  (5, _, R)           -> Pos (150, 151 - j) L
                  (5, _, D)           -> Pos (50, i + 100) L
                  (6, _, U)           -> p'
                  (6, _, R)           -> Pos (j - 100, 150) U
                  (6, _, L)           -> Pos (j - 100, 1) D
                  (6, _, D)           -> Pos (i + 100, 1) D
                  _                   -> error $ show p ++ " -> " ++ show p'
        debug p = show p ++ "(" ++ (show . cubeFace $ pos p) ++ ")"
          
    
parseCommands :: String -> [Command]
parseCommands []       = []
parseCommands ('R':xs) = CW:parseCommands xs
parseCommands ('L':xs) = CCW:parseCommands xs
parseCommands xs       = (Move n):parseCommands tail
  where [(n, tail)] = reads xs 

construct :: [String] -> Array.Array (Int, Int) Char
construct input = Array.array ((1,1),(width, height)) [((i, j), c) | (j, row) <- zip [1..] padded, (i, c) <- zip [1..] row]
  where height = length input
        width  = maximum $ map length input
        padded = map (take width . pad) input
        pad s  = s ++ repeat ' '

initialPosition :: Array.Array (Int, Int) Char -> Position
initialPosition map = Pos (head $ dropWhile (isEmpty map) $ zip [1..] (repeat 1)) R

travel :: (Position -> Position) -> Position -> [Command] -> Position
travel next p [] = p
travel next p (CW:xs)      = travel next (cw p) xs
travel next p (CCW:xs)     = travel next (ccw p) xs
travel next p (Move n:xs)  = travel next (head $ drop n $ iterate next p) xs

score :: Position -> Int
score (Pos (i, j) d) = 1000 * j + 4 * i + score' d
  where score' U = 3
        score' L = 2
        score' D = 1
        score' R = 0

solve :: (Position -> Position) -> Position -> [Command] -> Int
solve n p cs = score $ travel n p cs

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (mapText, "":[commandString]) = span (not . null) $ lines contents
  let map = construct mapText
  let commands = parseCommands commandString
  print $ solve (walk1 map) (initialPosition map) commands
  print $ solve (walk2 map) (initialPosition map) commands