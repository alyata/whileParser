module Expr where

import Tokenizer (tokenizer, tokenizerMatch)

import Text.Parsec
import Text.Read
import Numeric.Natural

data Expr 
  = Var String 
  | Const Natural 
  | Expr :*: Expr 
  | Expr :+: Expr
  deriving Eq

instance Show Expr where
  show (Var s) = s
  show (Const n) = show n
  show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"

-- nicer operators and values for building simple expressions
instance Num Expr where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger x = Const (fromInteger x)
  -- do not use these operations
  abs = undefined
  signum = undefined
  negate = undefined

x = Var "x"
y = Var "y"
z = Var "z"

parserExprBase :: Parsec String st Expr
parserExprBase = do
  t <- tokenizer
  case readMaybe t of
    Just n  -> return (Const n)
    Nothing -> return (Var t)

parserExprOp :: Parsec String st (Expr -> Expr -> Expr)
parserExprOp = do
  t <- tokenizer
  case t of
    "+" -> return (:+:)
    "*" -> return (:*:)
    _   -> unexpected $ "Unknown operator \"" ++ t ++ "\"."

parserExprInductive :: Parsec String st Expr
parserExprInductive = do
  tokenizerMatch (== "(")
  e1 <- parserExpr
  eOp <- try parserExprOp
  e2 <- parserExpr
  tokenizerMatch (== ")")
  return (eOp e1 e2)

parserExpr :: Parsec String st Expr
parserExpr = try parserExprInductive <|> parserExprBase

-- error if parsing stops before the end of the input
parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (parserExpr <* eof) "" s
