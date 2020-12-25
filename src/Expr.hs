module Expr where

import           Common
import           Template

import           Control.Applicative (liftA2)
import           Data.Map            (lookup)
import           Data.Maybe          (fromJust)
import           Prelude             hiding (lookup)
import           Text.LaTeX          hiding (between)
import           Text.Parsec         (ParseError, Parsec, alphaNum, between,
                                      chainl1, digit, eof, letter, many, many1,
                                      parse, spaces, (<?>), (<|>))

data Expr
  = Var String
  | Const Integer
  | Expr :*: Expr
  | Expr :+: Expr
    deriving Eq

instance Show Expr where
  show (Var s)     = s
  show (Const n)   = show n
  show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"

-- nicer operators and values for building simple expressions
instance Num Expr where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger n = Const (fromInteger n)
  -- do not use these operations
  abs = undefined
  signum = undefined
  negate = undefined

x :: Expr
x = Var "x"
y :: Expr
y = Var "y"
z :: Expr
z = Var "z"

instance Texy Expr where
  texy (Var var)   = fromString var
  texy (Const n)   = fromString (show n)
  texy (e1 :+: e2) = autoParens $ texy e1 <> fromString "+" <> texy e2
  texy (e1 :*: e2) = autoParens $ texy e1 `times` texy e2

-- |The 'variable' parser parses tokens consisting purely of alphanumeric
-- |letters to be a variable, except the first character has to be alphabetical.
variable :: Parsec String st String
variable = lexeme (liftA2 (:) letter (many alphaNum)) <?> "variable"

-- |The 'number' parser parses tokens consisting purely of digits 0-9 to be a
-- |natural number constant.
number :: Parsec String st Integer
number = read <$> lexeme (many1 digit) <?> "number"

-- |The 'atom' parser constructs parses an atomic expression, which may be a
-- |variable, constant or a bracketed expression. Bracketed expressions are
-- |atomic in the sense that they must be fully evaluated before their
-- |surroundings.
atom :: Parsec String st Expr
atom = Var <$> variable
   <|> Const <$> number
   <|> between (lexemeMatch "(" <?> "open parenthesis")
               (lexemeMatch ")" <?> "closing parenthesis")
               expr

-- |The 'exprOp' parser parses either a "+" or "*" as an expression operator.
-- |Not currently used as each operator has different precedence.
exprOp :: Parsec String st (Expr -> Expr -> Expr)
exprOp = (:+:) <$ lexemeMatch "+" <|> (:*:) <$ lexemeMatch "*"

-- |The 'precedence1' parser parses expressions at precedence level 1, which
-- |consists of atomic expressions separated by a "*".
precedence1 :: Parsec String st Expr
precedence1 = chainl1 atom ((:*:) <$ lexemeMatch "*")

-- |The 'precedence0' parser parses expressions at the base precedence level 0,
-- |which consists of precedence1 expressions separated by a "+". This means
-- |that addition has lower precedence than multiplication in parsing.
precedence0 :: Parsec String st Expr
precedence0 = chainl1 precedence1 ((:+:) <$ lexemeMatch "+")

-- |Parsing at the lowest precedence level is the same as parsing all
-- |expressions.
expr :: Parsec String st Expr
expr = precedence0 <?> "expression"

{- Precedence can be generalized as follows:

-- choice :: [Parsec t s a] -> Parsec t s a
-- choice = foldr (<|>) empty

-- Each precedence level is inhabited by a set of operations. They can be either
-- left or right associative.
data Op st a = LeftOp [Parsec String st (a -> a -> a)]
             | RightOp [Parsec String st (a -> a -> a)]

-- Given a list of precedence levels, and an atomic parser, generate the
-- complete parser
precedence :: [Op st a] -> Parsec String st a -> Parsec String st a
precedence ops atom = foldl convert atom ops
  where
    convert :: Parsec String st a -> Op st a -> Parsec String st a
    convert atom (LeftOp ops) = chainl1 atom (choice ops)
    convert atom (RightOp ops) = chainr1 atom (choice ops)

-- Then the same expr can be expressed as a call to the 'precedence' function
expr = precedence [ LeftOp [(:*:) <$ lexemeMatch "*"]
                  , LeftOp [(:+:) <$ lexemeMatch "+"]] atom
-}

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (spaces *> expr <* eof) "" s

-- | Evaluates an expression, returning the evaluation result and the proof tree
-- | generated (as a LaTeX equation)
evalExpr :: Expr -> State -> (Proof, Integer)
evalExpr e@(Var var) st = (druleExpr e st n proof, n)
  where
    n = fromJust $ lookup var (getMap st)
    proof = fromString var `in_` (texy st)
evalExpr e@(Const n) st = (druleExpr e st n mempty, n)
evalExpr e@(e1 :+: e2) st = (druleExpr e st (n1 + n2) (p1 <> p2), n1 + n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalExpr e@(e1 :*: e2) st = (druleExpr e st (n1 * n2) (p1 <> p2), n1 * n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st

druleExpr :: Expr -> State -> Integer -> Proof -> Proof
druleExpr e st n proof = drule proof (pre <> bse <> post)
  where
    pre = config e st
    post = config n st
