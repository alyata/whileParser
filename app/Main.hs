module Main where

import Expr (Expr(..), x)
import Boolean (Boolean(..))
import Latex (evalExpr, evalBoolean)
import Data.Map
import Text.LaTeX
import Text.LaTeX.Base.Render

main :: IO ()
main = do
  let proof = fst $ evalBoolean (T :|: (Not (x :>: 10))) (fromList [("x", 2)])
  renderFile "out.tex" proof
