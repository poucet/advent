module Main where
import System.Environment (getArgs)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import Text.Parsec ((<|>))
import Data.Word(Word16)
import Data.Bits
import qualified Data.Map as Map
import Data.Map ((!))

parse rule text = Parsec.parse rule "(source)" text

fromRight :: Either a b -> b
fromRight (Right x) = x

type Identifier = String
data Operand = Number Word16 | Identifier Identifier deriving (Eq, Show)
data Operator = OPAnd | OPOr | OPLShift | OPRShift deriving (Eq, Show)
data Operation = 
  Unary Operand |
  Binary Operator Operand Operand |
  Not Operand 
  deriving (Eq, Show)

data Instruction = Instruction {to :: Identifier, op :: Operation} deriving (Eq, Show)


parseIdentifier :: Parsec.Parsec String () String
parseIdentifier = Parsec.many1 Parsec.lower

parseNumber :: Parsec.Parsec String () Word16
parseNumber = read <$> Parsec.many1 Parsec.digit

parseOperand :: Parsec.Parsec String () Operand
parseOperand = (Identifier <$> Parsec.try parseIdentifier) <|> (Number <$> Parsec.try parseNumber)

parseNot :: Parsec.Parsec String () Operation
parseNot = Not <$> (Parsec.string "NOT" >> Parsec.spaces >> parseOperand)

parseBinary :: Parsec.Parsec String () Operation
parseBinary = do
  first <- parseOperand
  Parsec.spaces
  operation <- Parsec.many1 Parsec.upper
  Parsec.spaces
  second <- parseOperand
  case operation of
    "AND"    -> return $ Binary OPAnd first second
    "OR"     -> return $ Binary OPOr first second
    "RSHIFT" -> return $ Binary OPRShift first second
    "LSHIFT" -> return $ Binary OPLShift first second
    _        -> fail $ "Unknown operation: " ++ operation


parseOperation :: Parsec.Parsec String () Operation
parseOperation =  (Parsec.try parseBinary) <|> (Unary <$> Parsec.try parseOperand) <|> (Parsec.try parseNot)

parseInstruction :: Parsec.Parsec String () Instruction
parseInstruction = do
  operation <- parseOperation
  Parsec.spaces
  Parsec.string "->"
  Parsec.spaces
  id <- parseIdentifier
  return $ Instruction id operation

parseInstructions :: Parsec.Parsec String () [Instruction]
parseInstructions = do
  instruction <- parseInstruction
  instructions <- Parsec.try (Parsec.endOfLine >> parseInstructions) <|> return []
  return (instruction:instructions)


evaluateOperand :: Map.Map Identifier Word16 -> Operand -> Word16
evaluateOperand env (Number n) = n
evaluateOperand env (Identifier x) = env ! x

evaluateBinary :: Operator -> Word16 -> Word16 -> Word16
evaluateBinary OPAnd x y = x .&. y
evaluateBinary OPOr x y = x .|. y
evaluateBinary OPLShift x y = shiftL x (fromEnum y)
evaluateBinary OPRShift x y = shiftR x (fromEnum y)


evaluate :: Map.Map Identifier Word16 -> Operation -> Word16
evaluate env (Unary p) = evaluateOperand env p
evaluate env (Not p) = complement $ evaluateOperand env p
evaluate env (Binary op a b) = evaluateBinary op (evaluateOperand env a) (evaluateOperand env b)


problem1 :: [Instruction] -> Word16
problem1 instructions = mapping ! "a"
  where mapping = Map.fromList $ map (\i -> (to i, evaluate mapping $ op i)) instructions

problem2 :: [Instruction] -> Word16
problem2 instructions = mapping ! "a"
  where mapping = Map.fromList $ map (\i -> (to i, evaluate mapping $ op i)) (instructions ++ [Instruction "b" $ Unary $ Number $ problem1 instructions])

main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ problem1 $ fromRight $ parse parseInstructions contents
  print $ problem2 $ fromRight $ parse parseInstructions contents

