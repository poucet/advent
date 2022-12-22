module Main where
import System.Environment (getArgs)

process1 :: String -> Integer
process1 s = count s 0
  where count "" n = n
        count ('(':s') n = count s' (n+1)
        count (')':s') n = count s' (n-1)
        count ('\n':s') n = count s' n

process2 :: String -> Integer
process2 s = go s 0 0 
  where go _ c (-1) = c
        go ('(':s') c n = go s' (c+1) (n+1)
        go (')':s') c n = go s' (c+1) (n-1)
        go ('\n':s') c n = go s' c n

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ process1 contents
  print $ process2 contents