module Main where

import           Command            (evalCommand, parseCommand)
import           Common
import           Template           (template)

import           System.Environment (getArgs)
import           System.Exit        (ExitCode (ExitFailure), exitWith)
import           Text.LaTeX

main :: IO ()
main = do
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
