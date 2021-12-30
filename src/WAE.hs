module WAE where

import WAE.Definitions
import WAE.Parser
import WAE.Interpreter

program :: String
program = "{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ x y}}}"

runProgram = do
  code <- parse program
  return $ interp code

test = do
  putStrLn "Interpreting the WAE program:"
  putStrLn program
  print runProgram
