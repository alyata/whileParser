{-# LANGUAGE FlexibleContexts #-}

module Main where

-- the module to test
import Tokenizer (whitespaceParser, tokenParser)

-- other imports
import Text.Parsec
import Test.HUnit
import Data.Functor.Identity

main :: IO ()
main = do 
  runTestTT tests
  return ()

tests :: Test
tests = TestList [whitespaceParserTests, tokenParserTests]

whitespaceParserTests :: Test
whitespaceParserTests
  = TestList $ fmap (uncurry (testParser whitespaceParser))
      [("     wowwww", "     ")
      ,("\n\n\nQWE" , "\n\n\n")
      ]

tokenParserTests :: Test
tokenParserTests
  = TestList $ fmap (uncurry (testParser tokenParser))
      [("++++", "+") -- plus should be treated as 1 token on its own
      ,("******", "*") -- star should be treated as 1 token on its own
      ,("(aqwe)", "(") -- ( should be treated as 1 token on its own
      ,(")()", ")") -- ) should be treated as 1 token on its own
      ,("asdf qwer", "asdf") -- tokens are separated by whitespace
      ,("asdf*qwer", "asdf") -- tokens are separated by special character
      ]

testParser :: (Eq o, Show o, Show i, Stream i Identity t) => 
              Parsec i () o -> i -> o -> Test
testParser parser input expected
  = TestCase $ assertEqual ("parsing " ++ show input ++ ":")
                           (parse parser "" input)
                           (Right expected)
