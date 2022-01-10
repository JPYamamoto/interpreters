module Main where

import FWAE (interpreter)
import qualified CLI

main :: IO ()
main = CLI.buildMain interpreter
