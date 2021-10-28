module Main where

import AE
import WAE
import FWAE

main :: IO ()
main = do
  AE.test
  WAE.test
  FWAE.test
