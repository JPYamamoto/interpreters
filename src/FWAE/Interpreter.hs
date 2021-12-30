module FWAE.Interpreter (interp, desugar) where

import FWAE.Definitions

-- Semantic Analysis

-- Interpreter
interp :: SFWAE -> Maybe FAEResult
interp asa = interpExpr [] (desugar asa)

interpExpr :: Env -> FWAE -> Maybe FAEResult
interpExpr e (ID v)       = lookup v e
interpExpr _ (Num n)      = return $ Right n
interpExpr e (Op op exps) = do
  expsResults <- mapM (interpExpr e) exps
  return $ do
    result <- sequence expsResults
    return (operatorFun op result)
interpExpr e (Fun p body) = (return . Left) $ Closure p body e
interpExpr e (App e1 e2)  = do
  f <- interpExpr e e1
  x <- interpExpr e e2
  interpApp f x

interpApp :: FAEResult -> FAEResult -> Maybe FAEResult
interpApp (Left closure) arg = interpExpr ((cparam closure, arg):cenv closure) (cbody closure)
interpApp _              _   = Nothing

-- Desugar
desugar :: SFWAE -> FWAE
desugar (SID i)     = ID i
desugar (SNum n)    = Num n
desugar (SOp f exprs) = Op f $ map desugar exprs
desugar (SWith bs body)  = desugar $ withToApp bs body
desugar (SMWith bs body) = desugar $ mwithToWith bs body
desugar (SFun ps e)   = desugarFun ps (desugar e)
desugar (SApp e args) = desugarApp (desugar e) (map desugar args)

withToApp :: [SBinding] -> SFWAE -> SFWAE
withToApp bs body = let (ids, vals) = unzip bs
                    in SApp (SFun ids body) vals

mwithToWith :: [SBinding] -> SFWAE -> SFWAE
mwithToWith bs body = foldr (\b -> SWith [b]) body bs

desugarFun :: [String] -> FWAE -> FWAE
desugarFun ps body = foldr Fun body ps

desugarApp :: FWAE -> [FWAE] -> FWAE
desugarApp = foldl App
