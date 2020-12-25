{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Common where

import           Data.List             (intersperse)
import           Data.Map              (Map, toList)
import           Text.LaTeX
import           Text.LaTeX.Base.Class (fromLaTeX)
import           Text.Parsec           (Parsec, spaces, string, try)

type Proof = LaTeX
newtype Mapping = Mapping (String, Integer)
newtype State = State { getMap :: Map String Integer }

lexeme :: Parsec String st a -> Parsec String st a
lexeme p = try p <* spaces

lexemeMatch :: String -> Parsec String st ()
lexemeMatch s = lexeme (string s) *> return ()

-- LaTeX PRETTY PRINTERS --

instance Texy Mapping where
  texy (Mapping (var, val)) = fromString var <> mapsto <> texy val

instance Texy State where
  texy (State st) = fromLaTeX $ autoSquareBrackets (mconcat stElems)
    where
      stElems = intersperse "," $ map (texy.Mapping) (toList st)
