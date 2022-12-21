module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Data.Map as Map
import Text.Parsec ((<|>))

parse rule text = Parsec.parse rule "(source)" text

data Monkey = Monkey { name:: String, work :: Work} deriving (Eq, Show)

type Work = AST String

data AST a = 
  Unary Rational |
  Binary { op :: Char, left :: a, right :: a} |
  Variable String |
  Equality { leftEq :: a, rightEq :: a } deriving (Eq, Show)

data Value = Value {unValue :: (AST Value)} deriving (Eq, Show)


parseWork :: Parsec.Parsec String () Work
parseWork = (Parsec.try parseBinary) <|> (Parsec.try parseUnary)
  where parseBinary = do
                        left <- Parsec.many1 Parsec.lower
                        Parsec.spaces
                        op <- Parsec.oneOf "/*-+"
                        Parsec.spaces
                        right <- Parsec.many1 Parsec.lower
                        return $ Binary op left right
        parseUnary = Unary .  toRational . read <$> Parsec.many1 Parsec.digit


parseMonkey1 :: Parsec.Parsec String () Monkey
parseMonkey1 = do
  name <- Parsec.many1 Parsec.lower
  Parsec.char ':'
  Parsec.spaces
  work <- parseWork
  return $ Monkey name work

parseMonkey2 :: Parsec.Parsec String () Monkey
parseMonkey2 = do
  name <- Parsec.many1 Parsec.lower
  Parsec.char ':'
  Parsec.spaces
  work <- parseWork
  case name of
    "root" -> let (Binary _ left right ) = work in return $ Monkey name $ Equality left right
    "humn" -> return $ Monkey "humn" $ Variable "humn"
    _ -> return $ Monkey name work


evaluateOp :: Char -> Rational -> Rational -> Rational
evaluateOp '/' = (/)
evaluateOp '+' = (+)
evaluateOp '*' = (*)
evaluateOp '-' = (-)


evaluate1 :: Map.Map String Monkey -> String -> Rational
evaluate1 monkeys name =
  case work $ monkeys Map.! name of
    Unary i -> i
    Binary op left right -> evaluateOp op (evaluate1 monkeys left) (evaluate1 monkeys right)



evaluate2 :: Map.Map String Monkey -> String -> Value
evaluate2 monkeys name =
  case work $ monkeys Map.! name of
    Unary i -> Value $ Unary i
    Variable v -> Value $ Variable v
    Binary op left right ->
      let left' = (evaluate2 monkeys left)
          right' = (evaluate2 monkeys right) in
        case (unValue left', unValue right') of 
          ((Unary l), (Unary r)) -> Value $ Unary $ evaluateOp op l r
          (_, _)                             -> Value $ Binary op left' right'
    Equality left right -> Value $ Equality  (evaluate2 monkeys left)  (evaluate2 monkeys right)

simplify' :: (AST Value) -> (AST Value) -> Rational
simplify' (Unary x) (Variable y) = x
simplify' (Unary x) (Binary op y z) = 
  case (op, unValue y, unValue z) of
    ('+', Unary y', z') -> simplify' (Unary (x - y')) z'
    ('-', Unary y', z') -> simplify' (Unary (y' - x)) z'
    ('/', Unary y', z') -> simplify' (Unary (y' / x)) z'
    ('*', Unary y', z') -> simplify' (Unary (x / y')) z'
    ('+', y', Unary z') -> simplify' (Unary (x - z')) y'
    ('-', y', Unary z') -> simplify' (Unary (x + z')) y'
    ('/', y', Unary z') -> simplify' (Unary (x * z')) y'
    ('*', y', Unary z') -> simplify' (Unary (x / z')) y'

simplify' x y@(Unary _) = simplify' y x

simplify :: Value -> Rational
simplify (Value (Equality left right)) = simplify' (unValue left) (unValue right)



problem1 :: String -> IO ()
problem1 contents = do
  let monkeys = Map.fromList $ map ((\x -> (name x, x)) . fromRight . parse parseMonkey1) $ lines contents
  print $ evaluate1 monkeys "root"

problem2 :: String -> IO ()
problem2 contents = do
  let monkeys = Map.fromList $  map ((\x -> (name x, x)) . fromRight . parse parseMonkey2) $ lines contents
  print $ simplify $ evaluate2 monkeys "root"

fromRight (Right x) = x

main = do
  [filename] <- getArgs
  contents <- readFile filename
  problem1 contents
  problem2 contents