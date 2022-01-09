module Language (languageInterpreter) where

import Control.Monad ((>=>))
import Text.Parsec.Error (ParseError)

languageInterpreter :: (String -> Either ParseError c) -> (c -> Maybe v) -> (String -> Either ParseError (Maybe v))
languageInterpreter parser interpreter = parser >=> (return . interpreter)
