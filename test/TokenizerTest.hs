module TokenizerTest where

import Tokenizer (whitespaceParser, tokenParser)

import ParserTest (testParser)
import Test.HUnit

tests :: Test
tests = TestList [whitespaceParserTests, tokenParserTests]

whitespaceParserTests :: Test
whitespaceParserTests
  = TestList $ fmap (testParser whitespaceParser)
      [("     wowwww", "     ")
      ,("\n\n\nQWE" , "\n\n\n")]

tokenParserTests :: Test
tokenParserTests
  = TestList $ fmap (testParser tokenParser)
      [("++++", "+")          -- plus should be treated as 1 token on its own
      ,("******", "*")        -- star should be treated as 1 token on its own
      ,("(aqwe)", "(")        -- ( should be treated as 1 token on its own
      ,(")()", ")")           -- ) should be treated as 1 token on its own
      ,("asdf qwer", "asdf")  -- tokens are separated by whitespace
      ,("asdf*qwer", "asdf")] -- tokens are separated by special character

