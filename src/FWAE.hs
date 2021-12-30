module FWAE where

import FWAE.Definitions
import FWAE.Parser
import FWAE.Interpreter

program :: String
program = "{with {{x 3}} {with {{f {fun {y} {+ x y}}}} {with {{x 5}} {f 4}}}}"

runProgram = do
  code <- parse program
  return $ interp code

test = do
  putStrLn "Interpreting the FWAE program:"
  putStrLn program
  print runProgram
