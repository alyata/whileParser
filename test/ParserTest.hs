{-# LANGUAGE FlexibleContexts #-}

module ParserTest where

import Text.Parsec
import Test.HUnit
import Data.Functor.Identity

testParser :: (Eq o, Show o, Show i, Stream i Identity t) => 
              Parsec i () o -> (i, o) -> Test
testParser parser (input, expected)
  = TestCase $ assertEqual ("parsing " ++ show input ++ ":")
                           (parse parser "" input)
                           (Right expected)
