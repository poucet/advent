module Main where
import System.Environment (getArgs)

data Snafu = S [Char]

instance Show Snafu where
  show (S bits) = bits

fromS :: Char -> Int
fromS '1' = 1
fromS '2' = 2
fromS '0' = 0
fromS '-' = -1
fromS '=' = -2

toS :: Int -> Char
toS 1  = '1'
toS 2  = '2'
toS 0  = '0'
toS (-1) = '-' 
toS (-2) = '='

instance Enum Snafu where
  fromEnum (S bits) = run bits 0
    where run [] acc = acc
          run (x:xs) acc = run xs (5*acc + fromS x)
  toEnum i = S $ run i []
    where run 0 acc = acc
          run n acc = run n' ((toS (m - 2)):acc) 
            where (n', m) = (n + 2) `divMod` 5

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let snafus = map S $ lines $ contents
  print $ (toEnum $ sum $ map fromEnum $ snafus :: Snafu)