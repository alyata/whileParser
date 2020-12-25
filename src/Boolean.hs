module Boolean where

import           Common
import           Latex
import           Expr                (Expr, expr, evalExpr)

import           Control.Applicative ((<**>))
import           Text.LaTeX          hiding (between)
import           Text.Parsec         (ParseError, Parsec, between, chainl1, eof,
                                      parse, spaces, (<?>), (<|>))
import           Prelude             hiding (negate)

data Boolean
  = T | F
  | Expr :=: Expr
  | Expr :<: Expr
  | Expr :>: Expr
  | Boolean :&: Boolean
  | Boolean :|: Boolean
  | Not Boolean
    deriving Show

instance Texy Boolean where
  texy T           = true
  texy F           = false
  texy (b1 :&: b2) = autoParens $ texy b1 `wedge` texy b2
  texy (b1 :|: b2) = autoParens $ texy b1 `vee` texy b2
  texy (Not b1)    = autoParens $ neg <> texy b1
  texy (e1 :=: e2) = autoParens $ texy e1 <> "=" <> texy e2
  texy (e1 :<: e2) = autoParens $ texy e1 <> "<" <> texy e2
  texy (e1 :>: e2) = autoParens $ texy e1 <> ">" <> texy e2

truthVal :: Parsec String st Boolean
truthVal = T <$ lexemeMatch "true" <|> F <$ lexemeMatch "false"

boolBinOp :: Parsec String st (Boolean -> Boolean -> Boolean)
boolBinOp = (:&:) <$ lexemeMatch "&"
        <|> (:|:) <$ lexemeMatch "|"

exprBinOp :: Parsec String st (Expr -> Expr -> Boolean)
exprBinOp = (:=:) <$ lexemeMatch "="
        <|> (:<:) <$ lexemeMatch "<"
        <|> (:>:) <$ lexemeMatch ">"

negate :: Parsec String st (Boolean -> Boolean)
negate = Not <$ lexemeMatch "~"

atom :: Parsec String st Boolean
atom = truthVal
   <|> expr <**> exprBinOp <*> expr
   <|> between (lexemeMatch "(" <?> "open parenthesis")
               (lexemeMatch ")" <?> "closing parenthesis")
               boolean

literal :: Parsec String st Boolean
literal = atom <|> negate <*> atom

precedence1 :: Parsec String st Boolean
precedence1 = chainl1 literal ((:&:) <$ lexemeMatch "&")

precedence0 :: Parsec String st Boolean
precedence0 = chainl1 precedence1 ((:|:) <$ lexemeMatch "|")

boolean :: Parsec String st Boolean
boolean = precedence0 <?> "boolean"

parseBoolean :: String -> Either ParseError Boolean
parseBoolean = parse (spaces *> boolean <* eof) ""

-- | Evaluates a boolean, returning the evaluation result and the proof tree
-- | generated (as a LaTeX equation)
evalBoolean :: Boolean -> State -> (Proof, Bool)
evalBoolean T st
  = (druleBoolean T st True mempty, True)
evalBoolean F st
  = (druleBoolean F st False mempty, False)
evalBoolean b@(b1 :&: b2) st
  = (druleBoolean b st (b1' && b2') (p1 <> p2), b1' && b2')
  where
    (p1, b1') = evalBoolean b1 st
    (p2, b2') = evalBoolean b2 st
evalBoolean b@(b1 :|: b2) st
  = (druleBoolean b st (b1' || b2') (p1 <> p2), b1' || b2')
  where
    (p1, b1') = evalBoolean b1 st
    (p2, b2') = evalBoolean b2 st
evalBoolean b@(Not b1) st
  = (druleBoolean b st (not b1') p1, not b1')
  where
    (p1, b1') = evalBoolean b1 st
evalBoolean b@(e1 :=: e2) st
  = (druleBoolean b st (n1 == n2) (p1 <> p2), n1 == n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalBoolean b@(e1 :<: e2) st
  = (druleBoolean b st (n1 < n2) (p1 <> p2), n1 < n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalBoolean b@(e1 :>: e2) st
  = (druleBoolean b st (n1 > n2) (p1 <> p2), n1 > n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st

druleBoolean :: Boolean -> State -> Bool -> Proof -> Proof
druleBoolean b st b' proof
  = drule proof (pre <> bsb <> post)
  where
    pre = config b st
    post = config b' st
