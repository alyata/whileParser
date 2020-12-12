module Lib
    ( someFunc
    ) where

import Expr (Expr(..))

someFunc :: IO ()
someFunc = putStrLn $ show (Var "variable")
