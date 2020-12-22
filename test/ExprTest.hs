module ExprTest where

import Expr (expr, x, y, z)
import ParserTest (testParser)

import Test.HUnit

tests :: Test
tests 
  = TestList $ fmap (testParser expr) 
      [("(x + 1)", x + 1)
      ,("(x+1)", x + 1)
      ,("((   100 * x   ) +1)", (100 * x) + 1)
      ,("((1\n+\n10000) * \n y)", ((1+10000) * y))
      ,("(x + 1) ignore this stuff", (x + 1))]
