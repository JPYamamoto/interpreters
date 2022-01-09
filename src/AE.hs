module AE where

import AE.Parser
import AE.Interpreter
import Language (languageInterpreter)


interpreter = languageInterpreter parse (return . interp)
