module Main where

import WAE (interpreter)
import qualified CLI

main :: IO ()
main = CLI.buildMain interpreter
