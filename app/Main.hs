module Main where

import Expr (Expr(..), x)
import Boolean (Boolean(..))
import Latex (evalExpr, evalBoolean)
import Data.Map
import Text.LaTeX
import Text.LaTeX.Base.Render

main :: IO ()
main = do
  let st = fromList [("x", 2)]
      proof1 = fst $ evalBoolean (T :|: (Not (x :>: 10))) st
      proof2 = fst $ evalExpr ((x + 10) * 34 + (x * x * x * x)) st
  renderFile "out.tex" $ proof1 <> proof2
