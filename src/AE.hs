module AE where

import AE.Parser
import AE.Interpreter
import Language (buildInterpreter)


interpreter = buildInterpreter parse (return . interp)
