module Main where
import System.Environment (getArgs)
import Data.Sequence qualified as Seq
-- Based on https://github.com/glguy/advent/blob/main/solutions/src/2022/20.hs

grove :: (Eq a, Num a, Enum a) => Int -> [a] -> a
grove n xs = sum [Seq.index final ((i + j) `mod` Seq.length final) | j <- [1000,2000, 3000]]
    where final = run n xs
          Just i = Seq.elemIndexL 0 final


run :: (Eq a, Enum a) => Int -> [a] -> Seq.Seq a
run n input = go (Seq.fromList (zip [1..] input)) (concat (replicate n [1..length input]))
  where 
    go s []     = snd <$> s
    go s (x:xs) = go (Seq.insertAt d (x,v) (b <> a)) xs
      where 
        Just i = Seq.findIndexL (\t -> fst t == x) s
        (a, (_, v) Seq.:<| b) = Seq.splitAt i s
        d = (fromEnum v) `mod` (Seq.length s - 1)

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let numbers :: [Integer] = map read $ lines contents
  print (grove 1 numbers)
  print (grove 10 (map (811589153*) numbers))
