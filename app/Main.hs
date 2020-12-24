module Main where

import Expr (Expr(..), x)
import Boolean (Boolean(..))
import Command (Command(..), parseCommand)
import Latex (evalExpr, evalBoolean, evalCommand)
import Data.Map
import Text.LaTeX
import Text.LaTeX.Base.Render (renderFile)
import Text.LaTeX.Base.Parser (parseLaTeXFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Data.Either (fromRight)

main :: IO ()
main = do
  parseResult <- parseLaTeXFile "druleTemplate.tex"
  template <- eitherErrReturn "Failed to parse template" parseResult
  args <- getArgs
  progStr <- readFile $ args !! 0
  let p = parseCommand progStr
  prog <- eitherErrReturn (show p) (parseCommand progStr)
  let (proof, _) = evalCommand prog mempty
  renderFile "out.tex" $ template <> document (math proof)
  putStrLn "success!"

eitherErrReturn :: String -> Either a b -> IO b
eitherErrReturn msg 
  = either (\err -> putStrLn msg >> exitWith (ExitFailure 1)) return
