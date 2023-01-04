module Main where
import Debug.Trace(trace)
import System.Environment (getArgs)

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print contents
