module CFWAE where

import CFWAE.Parser
import CFWAE.Interpreter
import Language (languageInterpreter)

interpreter = languageInterpreter parse interp
