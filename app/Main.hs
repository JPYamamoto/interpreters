module Main where

import AE
import WAE
import FWAE
import CFWAE

main :: IO ()
main = do
  AE.test
  WAE.test
  FWAE.test
  CFWAE.test
