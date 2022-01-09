module FWAE where

import FWAE.Parser
import FWAE.Interpreter
import Language (languageInterpreter)


interpreter = languageInterpreter parse interp
