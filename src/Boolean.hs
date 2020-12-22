module Boolean where

import Tokenizer (tokenizer, tokenizerMatch)
import Expr (Expr, expr)

import Text.Parsec

data Boolean = 
  T | F | 
  Expr :=: Expr | Expr :<: Expr | Expr :>: Expr |
  Boolean :&: Boolean | Boolean :|: Boolean | Not Boolean
  deriving Show

parserBooleanBase :: Parsec String st Boolean
parserBooleanBase = do
  t <- tokenizer
  case t of
    "true" -> return T
    "false" -> return F
    _ -> unexpected $ "Unknown truth value."

parserBooleanExprOp :: Parsec String st (Expr -> Expr -> Boolean)
parserBooleanExprOp = do
  t <- tokenizer
  case t of
    "=" -> return (:=:)
    "<" -> return (:<:)
    ">" -> return (:>:)
    _ -> unexpected $ "Unknown boolean operator"

parserBooleanInductiveExprOp :: Parsec String st Boolean
parserBooleanInductiveExprOp = do
  e1 <- expr 
  bOp <- parserBooleanExprOp
  e2 <- expr 
  return (bOp e1 e2)

parserBooleanBinOp :: Parsec String st (Boolean -> Boolean -> Boolean)
parserBooleanBinOp = do
  t <- tokenizer
  case t of
    "&" -> return (:&:)
    "|" -> return (:|:)
    _ -> unexpected $ "Unknown boolean operator"

parserBooleanInductiveBinOp :: Parsec String st Boolean
parserBooleanInductiveBinOp = do
  b1 <- parserBoolean
  bOp <- parserBooleanBinOp
  b2 <- parserBoolean
  return (bOp b1 b2)

parserBooleanUnOp :: Parsec String st (Boolean -> Boolean)
parserBooleanUnOp = do
  t <- tokenizer
  case t of
    "~" -> return Not
    _ -> unexpected $ "Unknown boolean operator"

parserBooleanInductiveUnOp :: Parsec String st Boolean
parserBooleanInductiveUnOp = do
  bOp <- parserBooleanUnOp
  b <- parserBoolean
  return (bOp b)

parserBooleanInductive :: Parsec String st Boolean
parserBooleanInductive = do
  tokenizerMatch (== "(")
  *> (try parserBooleanInductiveExprOp 
  <|> try parserBooleanInductiveBinOp
  <|> parserBooleanInductiveUnOp)
  <* tokenizerMatch (== ")")

parserBoolean :: Parsec String st Boolean
parserBoolean = try parserBooleanInductive <|> try parserBooleanBase 
                <?> "This is not a valid boolean expression"

-- error if parsing stops before the end of the input
parseBoolean :: String -> Either ParseError Boolean
parseBoolean s = parse (parserBoolean <* eof) "" s
