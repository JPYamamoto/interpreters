module Main where

import CFWAE (interpreter)
import qualified CLI

main :: IO ()
main = CLI.buildMain interpreter
