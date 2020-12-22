module Main where

import qualified ExprTest   (tests)

import           Test.HUnit

main :: IO ()
main = do
  runTestTT $ TestList
    [ExprTest.tests]
  return ()
