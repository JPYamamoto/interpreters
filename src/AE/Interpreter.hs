module AE.Interpreter (interp) where

import AE.Definitions

-- Semantic Analysis
interp :: AE -> Number
interp (Num n) = n
interp (Add l r) = (interp l) + (interp r)
interp (Sub l r) = (interp l) - (interp r)
