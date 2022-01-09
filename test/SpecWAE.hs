module SpecWAE where

import Test.HUnit
import WAE

correct :: a -> Either b (Maybe a)
correct = return . return

test1 = TestCase (assertEqual ("Program: " ++ program) (interpreter program) (correct 17))
  where program = "{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ x y}}}"

tests = TestLabel "WAE Language" (TestList [test1])
