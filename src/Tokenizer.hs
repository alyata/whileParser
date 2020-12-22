module Tokenizer where

import Text.Parsec

whitespaceChars :: [Char]
whitespaceChars = " \n"

specialChars :: [Char]
specialChars = "+*&|<>=~"

whitespaceParser :: Parsec String st String
whitespaceParser = many $ oneOf whitespaceChars

-- a token is either a string of non-special/non-whitespace characters
-- or exactly one special character
tokenParser :: Parsec String st String
tokenParser = (do ch <- oneOf specialChars; return [ch])
             <|> (many1 $ noneOf (whitespaceChars ++ specialChars))

lexeme :: Parsec String st a -> Parsec String st a
lexeme p = try p <* whitespaceParser

tokenizer :: Parsec String st String
tokenizer = lexeme tokenParser

tokenizerMatch :: (String -> Bool) -> Parsec String st String
tokenizerMatch matcher = try $ do
  t <- tokenizer
  if matcher t
     then return t
     else fail "Token did not match."
