module Language ( buildInterpreter
                , Interpreter) where

import Control.Monad ((>=>))
import Text.Parsec.Error (ParseError)

type Interpreter v = String -> Either ParseError (Maybe v)

buildInterpreter :: (String -> Either ParseError c) -> (c -> Maybe v) -> Interpreter v
buildInterpreter parser interpreter = parser >=> (return . interpreter)
