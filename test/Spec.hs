import Test.HUnit

import qualified SpecAE as AE
import qualified SpecWAE as WAE
import qualified SpecFWAE as FWAE
import qualified SpecCFWAE as CFWAE

main = runTestTT (TestList [ AE.tests
                           , WAE.tests
                           , FWAE.tests
                           , CFWAE.tests
                           ])
