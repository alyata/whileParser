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
  -- Read and parse the program file
  progStr <- readFile $ args !! 0
  let p = parseCommand progStr
  prog <- eitherErrReturn (show p) (parseCommand progStr)
  -- Evaluate and render the parsed program
  let (proof, _) = evalCommand prog (State mempty)
      outFile = args !! 1
  renderFile outFile $ template <> document (math proof)
  putStrLn "success!"

eitherErrReturn :: String -> Either a b -> IO b
eitherErrReturn msg
  = either (\_ -> putStrLn msg >> exitWith (ExitFailure 1)) return
