{-# Language BlockArguments #-}
module Main where
import System.Environment (getArgs)
import Data.Array.ST (readArray, writeArray, MArray(newArray), runSTUArray)
import Data.Array.Unboxed (UArray, assocs)
import Control.Monad.Trans.Class ( MonadTrans(lift) )


firstHouse :: Int -> UArray Int Int -> Int
firstHouse n array = head [h | (h,t) <- assocs array, t >= n]

for :: Monad m => t -> (t -> Bool) -> (t -> t) -> (t -> m a) -> m ()
for init cond update f = 
  if not $ cond init then return ()
  else do
    f init
    for (update init) cond update f


populate1 :: Int -> UArray Int Int
populate1 n = runSTUArray do
  a <- newArray (1, top) 0
  for 1 (<= top) (1+) \elf -> do
    for elf (<= top) (elf+) \house -> do
      old <- readArray a house
      writeArray a house (old + elf * 10)
  return a
  where top = n `div` 10

populate2 :: Int -> UArray Int Int
populate2 n = runSTUArray do
  a <- newArray (1, top) 0
  for 1 (<= top) (1+) \elf -> do
    for elf (<= min top (elf*50)) (elf+) \house -> do
      old <- readArray a house
      writeArray a house (old + elf * 11)
  return a
  where top = n `div` 11


solve1 n = firstHouse n $ populate1 n

solve2 n = firstHouse n $ populate2 n

main :: IO ()
main = do
  n <- read . head <$> getArgs
  print $ solve1 n
  print $ solve2 n
