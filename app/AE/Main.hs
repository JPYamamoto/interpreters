module Main where

import AE (interpreter)
import qualified CLI

main :: IO ()
main = CLI.buildMain interpreter
