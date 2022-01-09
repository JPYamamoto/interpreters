module SpecFWAE where

import Test.HUnit
import FWAE

correct :: a -> Either b (Maybe a)
correct = return . return

test1 = TestCase (assertEqual ("Program: " ++ program) (interpreter program) (correct (Right 7)))
  where program =  "{with {{x 3}} {with {{f {fun {y} {+ x y}}}} {with {{x 5}} {f 4}}}}"

tests = TestLabel "FWAE Language" (TestList [test1])
