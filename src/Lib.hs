module Lib
    ( someFunc
    ) where

import Expr (parseExpr)
import Boolean (parseBoolean)

someFunc :: IO ()
someFunc = putStrLn $ show (parseBoolean "((1 < (1 + 10)) & (~ true))")
