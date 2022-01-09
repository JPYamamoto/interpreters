module SpecAE where

import Test.HUnit
import AE

correct :: a -> Either b (Maybe a)
correct = return . return

test1 = TestCase (assertEqual ("Program: " ++ program) (interpreter program) (correct 29))
  where program = "{+ {- 18 35} {+ 17 29}}"

tests = TestLabel "AE Language" (TestList [test1])
