module WAE where

import WAE.Parser
import WAE.Interpreter
import Language (languageInterpreter)


interpreter = languageInterpreter parse (return . interp)
