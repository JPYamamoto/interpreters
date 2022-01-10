module FWAE where

import FWAE.Parser
import FWAE.Interpreter
import Language (buildInterpreter)


interpreter = buildInterpreter parse interp
