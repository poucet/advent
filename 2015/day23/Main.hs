module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Data.Array as Array
import Data.Array ((!))
import qualified Data.Map as Map
import Text.Parsec ((<|>))
import GHC.Exts.Heap (GenClosure(value))
import qualified Data.IntMap as Array


parse rule text = Parsec.parse rule "(source)" text

type Offset = Int

data Register = A | B deriving (Eq, Ord, Show)

data Instruction  =
    Hlf Register        -- Half Register
  | Tpl Register        -- Triple Register
  | Inc Register        -- Increment Register
  | Jmp Offset          -- Jump
  | Jie Register Offset -- Jump if Even
  | Jio Register Offset -- Jump if One
    deriving (Eq, Ord, Show)

parseOffset :: Parsec.Parsec String () Offset
parseOffset = do
  sign <- Parsec.char '-' <|> Parsec.char '+'
  value <- Parsec.many1 Parsec.digit
  case sign of
    '+' -> return $ read value
    '-' -> return $ -1 * read value

parseRegister :: Parsec.Parsec String () Register
parseRegister = do
  reg <- Parsec.many1 Parsec.alphaNum
  case reg of
    "a" -> return A
    "b" -> return B
    _   -> error $ "Unknown register: " ++ reg

parseRegisterOffset :: (Register -> Offset -> Instruction) -> Parsec.Parsec String () Instruction
parseRegisterOffset ins = do
    Parsec.spaces
    reg <- parseRegister
    Parsec.string ","
    Parsec.spaces
    ins reg <$> parseOffset


parseInstruction :: Parsec.Parsec String () Instruction
parseInstruction = do
  ins <- Parsec.many1 Parsec.alphaNum
  case ins of
    "hlf" -> Hlf <$> (Parsec.spaces >> parseRegister)
    "tpl" -> Tpl <$> (Parsec.spaces >> parseRegister)
    "inc" -> Inc <$> (Parsec.spaces >> parseRegister)
    "jmp" -> Jmp <$> (Parsec.spaces >> parseOffset)
    "jie" -> parseRegisterOffset Jie
    "jio" -> parseRegisterOffset Jio

parseInstructions :: Parsec.Parsec String () [Instruction]
parseInstructions =
  Parsec.sepBy parseInstruction Parsec.newline

fromRight (Right x) = x

inrange :: (Ord a) => a -> (a, a) -> Bool
x `inrange` (l, h) = x >= l && x <= h

execute :: [Instruction] -> [(Register, Int)] -> Int
execute instructions regs = run (Map.fromList regs) 0
  where mem = Array.listArray (0, length instructions - 1) instructions
        bnd = Array.bounds mem 
        run regs p = 
          if not $ p `inrange` bnd
            then regs Map.! B 
            else 
              case mem ! p of
                Hlf r   -> run (Map.adjust (`div` 2) r regs) (p + 1)
                Tpl r   -> run (Map.adjust (* 3) r regs) (p + 1)
                Inc r   -> run (Map.adjust (+ 1) r regs) (p + 1)      
                Jmp o   -> run regs (p + o)
                Jie r o -> if (regs Map.! r) `mod` 2 == 0 then run regs (p + o) else run regs (p + 1)
                Jio r o -> if (regs Map.! r) == 1 then run regs (p + o) else run regs (p + 1)
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let instructions = fromRight $ parse parseInstructions contents
  print $ execute instructions [(A, 0), (B, 0)] 
  print $ execute instructions [(A, 1), (B, 0)] 
