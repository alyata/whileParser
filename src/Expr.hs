module Expr where

import Numeric.Natural

data Expr 
  = Var String 
  | Const Natural 
  | Expr :*: Expr 
  | Expr :+: Expr

instance Show Expr where
  show (Var s) = s
  show (Const n) = show n
  show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
