{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Latex where

import Expr (Expr(..))
import Boolean (Boolean(..))

import Text.LaTeX
import Numeric.Natural
import Data.Map hiding (map)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Text.LaTeX.Base.Syntax (TeXArg(FixArg), LaTeX(TeXComm, TeXCommS))
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, comm0, comm1, comm3)
import Data.List (intersperse)

type Proof = LaTeX
type State = Map String Natural

-- | Evaluates an expression, returning the evaluation result and the proof tree
-- | generated (as a LaTeX equation)
evalExpr :: Expr -> State -> (Proof, Natural)
evalExpr e@(Var id) st 
  = (druleExpr e st n (fromString id `in_` (texy st)), n)
  where
    n = fromJust (lookup id st)
evalExpr e@(Const n) st
  = (druleExpr e st n mempty, n)
evalExpr e@(e1 :+: e2) st
  = (druleExpr e st (n1 + n2) (p1 <> p2), n1 + n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalExpr e@(e1 :*: e2) st
  = (druleExpr e st (n1 * n2) (p1 <> p2), n1 * n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st

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

druleExpr :: Expr -> State -> Natural -> Proof -> Proof
druleExpr e st n proof = drule proof (pre <> bse <> post)
  where
    pre = config e st
    post = config n st

config :: Texy a => a -> State -> LaTeX
config val st = ang $ texy val <> fromString ", " <> texy st

-- CUSTOM COMMANDS FROM TEMPLATE --

drule :: LaTeXC l => l -> l -> l
drule top bottom = comm3 "drule" "" top bottom

ang :: LaTeXC l => l -> l
ang content = comm1 "ang" content

bse :: LaTeXC l => l
bse = comm0 "bse"

bsb :: LaTeXC l => l
bsb = comm0 "bsb"

true :: LaTeXC l => l
true = comm0 "true"

false :: LaTeXC l => l
false = comm0 "false"

neg :: LaTeXC l => l
neg = comm0 "neg"

-- LaTeX PRETTY PRINTERS --

instance Texy (String, Natural) where
  texy (id, val) = fromString id <> mapsto <> texy val

instance Texy State where
  texy st = fromLaTeX $ autoSquareBrackets (mconcat stElems)
    where
      stElems = intersperse "," $ map texy (toList st)

instance Texy Expr where
  texy (Var id) = fromString id
  texy (Const n) = fromString (show n)
  texy (e1 :+: e2) = autoParens $ texy e1 <> fromString "+" <> texy e2
  texy (e1 :*: e2) = autoParens $ texy e1 `times` texy e2

instance Texy Natural where
  texy n = fromString (show n)

instance Texy Boolean where
  texy T = true
  texy F = false
  texy (b1 :&: b2) = autoParens $ texy b1 `wedge` texy b2
  texy (b1 :|: b2) = autoParens $ texy b1 `vee` texy b2
  texy (Not b1) = autoParens $ neg <> texy b1
  texy (e1 :=: e2) = autoParens $ texy e1 <> fromString "=" <> texy e2
  texy (e1 :<: e2) = autoParens $ texy e1 <> fromString "<" <> texy e2
  texy (e1 :>: e2) = autoParens $ texy e1 <> fromString ">" <> texy e2
