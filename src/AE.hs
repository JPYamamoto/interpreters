module AE where

import AE.Definitions
import AE.Parser
import AE.Interpreter

program :: String
program = "{+ {- 18 35} {+ 17 29}}"

runProgram = do
  code <- parse program
  return $ interp code

test = do
  putStrLn "Interpreting the AE program:"
  putStrLn program
  print runProgram
