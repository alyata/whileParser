module Main where

import qualified TokenizerTest (tests)
import qualified ExprTest (tests)

import Test.HUnit

main :: IO ()
main = do 
  runTestTT $ TestList 
    [TokenizerTest.tests
    ,ExprTest.tests]
  return ()
