module SpecCFWAE where

import Test.HUnit
import CFWAE
import CFWAE.Definitions

correct :: a -> Either b (Maybe a)
correct = return . return

test1 = TestCase (assertEqual ("Program: " ++ program) (interpreter program) (correct (VBool False)))
  where program =   "{with {{p true} {q false}} {not {and p {or p q}}}}"

tests = TestLabel "CFWAE Language" (TestList [test1])
