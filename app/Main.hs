module Main where

import Common
import Command (parseCommand, evalCommand)

import Text.LaTeX
import Text.LaTeX.Base.Parser (parseLaTeXFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

main :: IO ()
main = do
  parseResult <- parseLaTeXFile "druleTemplate.tex"
  template <- eitherErrReturn "Failed to parse template" parseResult
  args <- getArgs
  progStr <- readFile $ args !! 0
  let p = parseCommand progStr
  prog <- eitherErrReturn (show p) (parseCommand progStr)
  let (proof, _) = evalCommand prog (State mempty)
  renderFile "out.tex" $ template <> document (math proof)
  putStrLn "success!"

eitherErrReturn :: String -> Either a b -> IO b
eitherErrReturn msg 
  = either (\_ -> putStrLn msg >> exitWith (ExitFailure 1)) return
