module Command where

import           Common
import Latex
import           Expr                (Expr, expr, variable, evalExpr)
import           Boolean             (Boolean, boolean, evalBoolean)

import           Control.Applicative ((<**>))
import           Text.LaTeX          hiding (between)
import           Text.Parsec         (ParseError, Parsec, between, chainr1, eof,
                                      parse, spaces, (<?>), (<|>))
import           Data.Map               (insert)

data Command =
  String ::=: Expr | If Boolean Command Command |
  Command ::: Command | Skip | While Boolean Command
  deriving Show

instance Texy Command where
  texy (var ::=: e)  = fromString var <> ":=" <> texy e
  texy (If b c1 c2) = ifthen (texy b) (texy c1) (texy c2)
  texy (c1 ::: c2)  = texy c1 <> ";" <> texy c2
  texy Skip         = texttt "skip"
  texy (While b c)  = while (texy b) (texy c)

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

whileC :: Parsec String st Command
whileC = (While <$ lexemeMatch "while") <*> boolean
    <*> (lexemeMatch "do" *> singleCommand)

singleCommand :: Parsec String st Command
singleCommand = ifte <|> whileC <|> atom

command :: Parsec String st Command
command = chainr1 singleCommand ((:::) <$ lexemeMatch ";") <?> "command"

parseCommand :: String -> Either ParseError Command
parseCommand = parse (spaces *> command <* eof) ""

evalCommand :: Command -> State -> (Proof, State)
evalCommand Skip st = (druleCommand Skip st st mempty, st)
evalCommand c@(var ::=: e) st = (druleCommand c st st' p, st')
  where
    (p, n) = evalExpr e st
    st' = State $ insert var n (getMap st)
evalCommand c@(If b c1 c2) st = (druleCommand c st st' (pb <> p), st')
  where
    (pb, b') = evalBoolean b st
    (p, st') = evalCommand (if b' then c1 else c2) st
evalCommand c@(c1 ::: c2) st = (druleCommand c st st'' (p1 <> p2), st'')
  where
    (p1, st') = evalCommand c1 st
    (p2, st'') = evalCommand c2 st'
evalCommand c@(While b c1) st = (druleCommand c st st'' (pb <> p <> pw), st'')
  where
    (pb, b') = evalBoolean b st
    (p, st') = if b' then evalCommand c1 st else (mempty, st)
    (pw, st'') = if b' then evalCommand c st' else (mempty, st)

druleCommand :: Command -> State -> State -> Proof -> Proof
druleCommand c st st' proof = drule proof (pre <> bsc <> post)
  where
    pre = config c st
    post = texy st'
