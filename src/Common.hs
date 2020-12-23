module Common where

import           Text.Parsec (Parsec, spaces, string, try)

lexeme :: Parsec String st a -> Parsec String st a
lexeme p = try p <* spaces

lexemeMatch :: String -> Parsec String st ()
lexemeMatch s = lexeme (string s) *> return ()
