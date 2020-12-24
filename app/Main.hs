module Main where

import Expr (Expr(..), x)
import Boolean (Boolean(..))
import Command (Command(..))
import Latex (evalExpr, evalBoolean, evalCommand)
import Data.Map
import Text.LaTeX
import Text.LaTeX.Base.Render

main :: IO ()
main = do
  let st = fromList [("x", 2)]
      --proof1 = fst $ evalBoolean (T :|: (Not (x :>: 10))) st
      --proof2 = fst $ evalExpr ((x + 10) * 34 + (x * x * x * x)) st
      proof3 = fst $ evalCommand (While (x :<: 20) ("x" ::=: (x + 1))) st
  renderFile "out.tex" $ proof3
