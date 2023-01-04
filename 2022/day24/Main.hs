module Main where
import Debug.Trace(trace)
import System.Environment (getArgs)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.Unboxed qualified as UArray
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Ix(Ix(unsafeIndex, range, index, inRange, unsafeRangeSize), indexError)


data Coord = C !Int !Int deriving (Eq, Ord, Show)
type Grid a = UArray Coord a

zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (C x1 y1) (C x2 y2) = C (f x1 x2) (f y1 y2)

mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (C x y) = C (f x) (f y)

instance Num Coord where
  (+) = zipCoord (+)
  {-# INLINE (+) #-}
  (*) = zipCoord (*)
  {-# INLINE (*) #-}
  fromInteger = (\i -> C i i) . fromInteger
  {-# INLINE fromInteger #-}
  signum = mapCoord signum
  {-# INLINE signum #-}
  abs = mapCoord abs
  {-# INLINE abs #-}
  negate = mapCoord negate
  {-# INLINE negate #-}

instance Ix Coord where
  range ((C x1 y1),(C x2 y2)) = [(C xi yi) | xi <- range(x1,x2), yi <- range (y1,y2)]
  {-# INLINE range #-}

  inRange ((C xl yl), (C xh yh)) (C x y) = inRange (xl, xh) x && inRange (yl, yh) y
  {-# INLINE inRange #-}

  unsafeRangeSize (C xl yl, C xh yh) = (xh - xl + 1) * (yh - yl + 1)
  {-# INLINE unsafeRangeSize #-}

  unsafeIndex (C xl yl, C xh yh) (C x y) = 
      unsafeIndex (yl,yh) y * unsafeRangeSize (xl,xh) + unsafeIndex (xl, xh) x
  {-# INLINE unsafeIndex #-}

  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = indexError b i "Coord"

cardinal (C x y) = [C (x-1) y, C (x+1) y, C x (y-1), C x (y+1)]


data State = S { 
  time :: !Int,
  locations :: !(Set Coord)
}


toGrid :: (UArray.IArray UArray a) => [[a]] -> Grid a
toGrid []   = error "Could not create empty grid"
toGrid rows = UArray.array (C 1 1, C width height) [(C i j, c) | (j, row) <- zip [1..height] rows, (i, c) <- zip [1..width] row]
    where (height, width) = (length rows, length $ head rows)

parse :: String -> IO (Grid Char)
parse filename = do
  contents <- readFile filename
  return $ toGrid $ lines contents

step grid (S t ls) = (S (t+1)) $ Set.fromList [
  next | here <- Set.toList ls,
         next <- here : cardinal here,
         canExist grid (t+1) next
  ]

canExist ::  Grid Char -> Int -> Coord -> Bool
canExist grid time p@(C x y) = 
  inRange bounds p             &&
  (grid ! p) /= '#'            &&
  (grid ! (C eastx y)) /= '>'  &&
  (grid ! (C westx y)) /= '<'  &&
  (grid ! (C x northy)) /= '^' &&
  (grid ! (C x southy)) /= 'v'         
  where bounds@(C xl yl, C xh yh) = (UArray.bounds grid)
        eastx  = (xl + 1) + ((x - time - (xl + 1)) `mod` (xh - xl - 1))
        westx  = (xl + 1) + ((x + time - (xl + 1)) `mod` (xh - xl - 1))
        northy = (yl + 1) + ((y + time - (yl + 1)) `mod` (yh - yl - 1))
        southy = (yl + 1) + ((y - time - (yl + 1)) `mod` (yh - yl - 1))

travelTo :: Grid Char -> Coord -> State -> State
travelTo grid dest = head . dropWhile (not . Set.member dest . locations) . iterate (step grid) 


main = do
  [filename] <- getArgs
  grid <- parse filename
  let (C maxX maxY) = snd $ UArray.bounds grid
  let begin = (C 2 1)
  let end = C (maxX - 1) (maxY)
  let t'  = time $ travelTo grid end $ S 0 $ Set.singleton begin
  let t'' = time $ travelTo grid begin $ S t' $ Set.singleton end
  let t''' = time $ travelTo grid end $ S t'' $ Set.singleton begin
  print (t', t'', t''')
  

