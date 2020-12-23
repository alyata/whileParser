module Command where

import           Common              (lexemeMatch)
import           Expr                (Expr, expr, variable)

import           Boolean             (Boolean, boolean)
import           Control.Applicative ((<**>))
import           Text.Parsec         (ParseError, Parsec, between, chainr1, eof,
                                      parse, spaces, (<?>), (<|>))

data Command =
  String ::=: Expr | If Boolean Command Command |
  Command ::: Command | Skip | While Boolean Command
  deriving Show

atom :: Parsec String st Command
atom = (Skip <$ lexemeMatch "skip")
   <|> variable <**> ((::=:) <$ lexemeMatch ":=") <*> expr
   <|> between (lexemeMatch "{" <?> "open bracket")
               (lexemeMatch "}" <?> "close bracket")
               command

ifte :: Parsec String st Command
ifte = (If <$ lexemeMatch "if") <*> boolean
   <*> (lexemeMatch "then" *> singleCommand)
   <*> (lexemeMatch "else" *> singleCommand)

while :: Parsec String st Command
while = (While <$ lexemeMatch "while") <*> boolean
    <*> (lexemeMatch "do" *> singleCommand)

singleCommand :: Parsec String st Command
singleCommand = ifte <|> while <|> atom

command :: Parsec String st Command
command = chainr1 singleCommand ((:::) <$ lexemeMatch ";") <?> "command"

parseCommand :: String -> Either ParseError Command
parseCommand = parse (spaces *> command <* eof) ""
