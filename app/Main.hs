module Main where

import Expr (parseExpr)
import Boolean (parseBoolean)

main :: IO ()
main = putStrLn $ show (parseBoolean "~true&1>1|false")

