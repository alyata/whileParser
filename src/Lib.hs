module Lib
    ( someFunc
    ) where

import Expr (parseExpr)
import Boolean (parseBoolean)

someFunc :: IO ()
someFunc = putStrLn $ show (parseExpr "x + 10 * 20")
