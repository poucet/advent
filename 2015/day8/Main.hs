module Main where
import System.Environment (getArgs)

countEscaped :: String -> Int
countEscaped s = doCount 0 s
  where doCount n ""                  = n
        doCount n ('\\':'\\':xs)      = doCount (n+1) xs 
        doCount n ('\\':'"':xs)       = doCount (n+1) xs 
        doCount n ('\\':'x':a:b:xs)   = doCount (n+1) xs 
        doCount n (x:xs)              = doCount (n+1) xs 

countChars :: String -> Int
countChars s | head s == '"' && last s == '"' = countEscaped s - 2
countChars s | otherwise                      = error "Invalid"

problem1 :: [String] -> IO ()
problem1 ls =
  print $ (sum $ map length ls) - (sum $ map countChars ls)
  where read' :: String -> String
        read' l = read l

problem2 :: [String] -> IO ()
problem2 ls =
  print $ (sum $ map (length . show) ls) - (sum $ map length ls) 
  where read' :: String -> String
        read' l = read l

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let l = lines contents
  problem1 l
  problem2 l