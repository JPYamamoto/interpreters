module FWAE.Interpreter (interp) where

import FWAE.Definitions
import Data.Maybe
import Data.Either

type FAEResult = Either Closure Number
type Env = [(String, FAEResult)]
data Closure = Closure {cparam :: String, cbody :: FWAE, env :: Env}

instance Show Closure where
  show (Closure p b _) = show $ Fun p b

-- Semantic Analysis
interp :: FWAE -> Maybe FAEResult
interp asa = interpExpr [] (desugar asa)

interpExpr :: Env -> FWAE -> Maybe FAEResult
interpExpr e (ID v)    = lookup v e
interpExpr _ (Num n)   = return $ Right n
interpExpr e (Add l r) = do
  lresult <- interpExpr e l
  rresult <- interpExpr e r
  interpOp (+) lresult rresult
interpExpr e (Sub l r) = do
  lresult <- interpExpr e l
  rresult <- interpExpr e r
  interpOp (-) lresult rresult
interpExpr e (Fun p e1)  = (return . Left) $ Closure p e1 e
interpExpr e (App e1 e2) = do
  f <- interpExpr e e1
  x <- interpExpr e e2
  interpApp f x
interpExpr _ _ = error "Unreachable Statement"

interpOp :: (Number -> Number -> Number) -> FAEResult -> FAEResult -> Maybe FAEResult
interpOp f (Right l) (Right r) = (return . return) $ f l r
interpOp _ _         _         = Nothing

interpApp :: FAEResult -> FAEResult -> Maybe FAEResult
interpApp (Left closure) arg = interpExpr ((cparam closure, arg):env closure) (cbody closure)
interpApp _              _   = Nothing

desugar :: FWAE -> FWAE
desugar e@(ID _)    = e
desugar e@(Num _)   = e
desugar (Add l r)   = Add (desugar l) (desugar r)
desugar (Sub l r)   = Sub (desugar l) (desugar r)
desugar (With wID wVal wBody) = desugar $ App (Fun wID wBody) wVal
desugar (Fun p e)   = Fun p (desugar e)
desugar (App e1 e2) = App (desugar e1) (desugar e2)
