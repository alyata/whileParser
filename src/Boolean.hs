module Boolean where

import           Common              (lexemeMatch)
import           Expr                (Expr, expr)

import           Control.Applicative ((<**>))
import           Text.Parsec         (ParseError, Parsec, between, chainl1, eof,
                                      parse, spaces, (<?>), (<|>))

data Boolean
  = T | F
  | Expr :=: Expr
  | Expr :<: Expr
  | Expr :>: Expr
  | Boolean :&: Boolean
  | Boolean :|: Boolean
  | Not Boolean
    deriving Show

truthVal :: Parsec String st Boolean
truthVal = T <$ lexemeMatch "true" <|> F <$ lexemeMatch "false"

boolBinOp :: Parsec String st (Boolean -> Boolean -> Boolean)
boolBinOp = (:&:) <$ lexemeMatch "&"
        <|> (:|:) <$ lexemeMatch "|"

exprBinOp :: Parsec String st (Expr -> Expr -> Boolean)
exprBinOp = (:=:) <$ lexemeMatch "="
        <|> (:<:) <$ lexemeMatch "<"
        <|> (:>:) <$ lexemeMatch ">"

neg :: Parsec String st (Boolean -> Boolean)
neg = Not <$ lexemeMatch "~"

atom :: Parsec String st Boolean
atom = truthVal
   <|> expr <**> exprBinOp <*> expr
   <|> between (lexemeMatch "(" <?> "open parenthesis")
               (lexemeMatch ")" <?> "closing parenthesis")
               boolean

literal :: Parsec String st Boolean
literal = atom <|> neg <*> atom

precedence1 :: Parsec String st Boolean
precedence1 = chainl1 literal ((:&:) <$ lexemeMatch "&")

precedence0 :: Parsec String st Boolean
precedence0 = chainl1 precedence1 ((:|:) <$ lexemeMatch "|")

boolean :: Parsec String st Boolean
boolean = precedence0 <?> "boolean"

parseBoolean :: String -> Either ParseError Boolean
parseBoolean = parse (spaces *> boolean <* eof) ""
