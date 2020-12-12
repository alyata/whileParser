module Lib
    ( someFunc
    ) where

import Expr (Expr(..), parseExpr)

someFunc :: IO ()
someFunc = putStrLn $ show (parseExpr "(1*(3+2))")
