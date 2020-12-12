module Main where

import qualified TokenizerTest
import qualified ExprTest

import Test.HUnit

main :: IO ()
main = do 
  runTestTT $ TestList 
    [TokenizerTest.tests
    ,ExprTest.tests]
  return ()
