module CFWAE where

import CFWAE.Parser
import CFWAE.Interpreter
import Language (buildInterpreter)


interpreter = buildInterpreter parse interp
