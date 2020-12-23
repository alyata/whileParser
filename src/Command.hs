module Command where

import           Common              (lexemeMatch)
import           Expr                (Expr, expr, variable)

import           Boolean             (Boolean, boolean)
import           Control.Applicative ((<**>))
import           Text.Parsec         (ParseError, Parsec, chainr1, eof, parse,
                                      spaces, (<?>), (<|>))

data Command =
  String ::=: Expr | If Boolean Command Command |
  Command ::: Command | Skip | While Boolean Command
  deriving Show

atom :: Parsec String st Command
atom = (Skip <$ lexemeMatch "skip")
   <|> variable <**> ((::=:) <$ lexemeMatch ":=") <*> expr

ifte :: Parsec String st Command
ifte = (If <$ lexemeMatch "if") <*> boolean
   <*> (lexemeMatch "then" *> command)
   <*> (lexemeMatch "else" *> command)

while :: Parsec String st Command
while = (While <$ lexemeMatch "while") <*> boolean
    <*> (lexemeMatch "do" *> command)

command :: Parsec String st Command
command = chainr1 (ifte <|> while <|> atom) ((:::) <$ lexemeMatch ";") <?> "command"

parseCommand :: String -> Either ParseError Command
parseCommand = parse (spaces *> command <* eof) ""
