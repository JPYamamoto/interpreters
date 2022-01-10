module WAE where

import WAE.Parser
import WAE.Interpreter
import Language (buildInterpreter)


interpreter = buildInterpreter parse (return . interp)
