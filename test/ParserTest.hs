{-# LANGUAGE FlexibleContexts #-}

module ParserTest where

import           Data.Functor.Identity
import           Test.HUnit
import           Text.Parsec

testParser :: (Eq o, Show o, Show i, Stream i Identity t) =>
              Parsec i () o -> (i, o) -> Test
testParser parser (input, expected)
  = TestCase $ assertEqual ("parsing " ++ show input ++ ":")
                           (parse parser "" input)
                           (Right expected)
