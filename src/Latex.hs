{-# LANGUAGE OverloadedStrings #-}

module Latex where

import Expr (Expr(..))
import Boolean (Boolean(..))

import Text.LaTeX
import Numeric.Natural
import Data.Map hiding (map)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Text.LaTeX.Base.Syntax (TeXArg(FixArg), LaTeX(TeXComm, TeXCommS))
import Data.List (intersperse)

type Proof = LaTeX
type State = Map String Natural

evalExpr :: Expr -> State -> (Proof, Natural)
evalExpr e@(Var id) st 
  = (druleE e st n (fromString id `in_` (pprintSt st)), n)
  where
    n = fromJust (lookup id st)
evalExpr e@(Const n) st
  = (druleE e st n mempty, n)
evalExpr e@(e1 :+: e2) st
  = (druleE e st (n1 + n2) (p1 <> p2), n1 + n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalExpr e@(e1 :*: e2) st
  = (druleE e st (n1 * n2) (p1 <> p2), n1 * n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st

evalBoolean :: Boolean -> State -> (Proof, Bool)
evalBoolean T st 
  = (druleB T st True mempty, True)
evalBoolean F st 
  = (druleB F st False mempty, False)
evalBoolean b@(b1 :&: b2) st 
  = (druleB b st (b1' && b2') (p1 <> p2), b1' && b2')
  where
    (p1, b1') = evalBoolean b1 st
    (p2, b2') = evalBoolean b2 st
evalBoolean b@(b1 :|: b2) st 
  = (druleB b st (b1' || b2') (p1 <> p2), b1' || b2')
  where
    (p1, b1') = evalBoolean b1 st
    (p2, b2') = evalBoolean b2 st
evalBoolean b@(Not b1) st
  = (druleB b st (not b1') p1, not b1')
  where
    (p1, b1') = evalBoolean b1 st
evalBoolean b@(e1 :=: e2) st
  = (druleB b st (n1 == n2) (p1 <> p2), n1 == n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalBoolean b@(e1 :<: e2) st
  = (druleB b st (n1 < n2) (p1 <> p2), n1 < n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st
evalBoolean b@(e1 :>: e2) st
  = (druleB b st (n1 > n2) (p1 <> p2), n1 > n2)
  where
    (p1, n1) = evalExpr e1 st
    (p2, n2) = evalExpr e2 st

druleB :: Boolean -> State -> Bool -> Proof -> Proof
druleB b st b' proof
  = drule proof (pre <> bsb <> post)
  where
    pre = config b st
    post = config b' st

druleE :: Expr -> State -> Natural -> Proof -> Proof
druleE e st n proof 
  = drule proof (pre <> bse <> post)
  where
    pre = config e st
    post = config n st

config :: Show a => a -> Map String Natural -> LaTeX
config val st = ang $ (fromString $ show val) 
             <> (fromString ", ") 
             <> (pprintSt st)


-- CUSTOM COMMANDS FROM TEMPLATE --

drule :: LaTeX -> LaTeX -> LaTeX
drule top bottom = TeXComm "drule" [FixArg "", FixArg top, FixArg bottom]

ang :: LaTeX -> LaTeX
ang content = TeXComm "ang" [FixArg content]

bse :: LaTeX
bse = TeXCommS "bse"

bsb :: LaTeX
bsb = TeXCommS "bsb"

-- LaTeX PRETTY PRINTERS --

pprintStElem :: (String, Natural) -> LaTeX
pprintStElem (id, val) 
  = fromString id <> mapsto <> fromString (show val)

pprintSt :: State -> LaTeX
pprintSt st = autoSquareBrackets (mconcat stElems)
  where
    stElems = intersperse "," $ map pprintStElem (toList st)
