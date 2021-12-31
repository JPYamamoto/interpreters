module CFWAE where

import CFWAE.Definitions
import CFWAE.Parser
import CFWAE.Interpreter

program :: String
program = "{with {{p true} {q false}} {not {and p {or p q}}}}"

runProgram = do
  code <- parse program
  return $ interp code

test = do
  putStrLn "Interpreting the CFWAE program:"
  putStrLn program
  print runProgram
