module CFWAE.Interpreter (interp, desugar) where

import CFWAE.Definitions

-- Semantic Analysis

-- Interpreter
interp :: SCFWAE -> Maybe Value
interp asa = interpExpr [] (desugar asa)

interpExpr :: Env -> CFWAE -> Maybe Value
interpExpr e (ID v)       = lookup v e
interpExpr _ (Num n)      = return $ VNum n
interpExpr _ (Bool b)     = return $ VBool b
interpExpr e (Op op exps) = interpOp e op exps
interpExpr e (If testE thenE elseE) = interpIf e testE thenE elseE
interpExpr e (Fun p body) = (return . VClosure) $ Closure p body e
interpExpr e (App e1 e2)  = do
  f <- interpExpr e e1
  x <- interpExpr e e2
  interpApp f x

interpOp :: Env -> Operator -> [CFWAE] -> Maybe Value
interpOp e (Arithmetic op) exprs = do
  expsResults <- mapM (interpExpr e) exprs
  if all isVNum expsResults
    then (return . VNum) $ (arithOperatorFun op) (map vnumValue expsResults)
    else Nothing
interpOp e (Logical op) exprs = do
  expsResults <- mapM (interpExpr e) exprs
  if all isVBool expsResults
    then (return . VBool) $ (logicalOperatorFun op) (map vboolValue expsResults)
    else Nothing
interpOp e (Relational op) exprs = do
  expsResults <- mapM (interpExpr e) exprs
  if all isVNum expsResults
    then (return . VBool) $ (relationalOperatorFun op) (map vnumValue expsResults)
    else Nothing

interpIf :: Env -> CFWAE -> CFWAE -> CFWAE -> Maybe Value
interpIf e testE thenE elseE = do
  testResult <- interpExpr e testE
  result <- case testResult of
    (VBool True) -> interpExpr e thenE
    (VBool False) -> interpExpr e elseE
    otherwise -> Nothing

  return result

interpApp :: Value -> Value -> Maybe Value
interpApp (VClosure closure) arg = interpExpr ((cparam closure, arg):cenv closure) (cbody closure)
interpApp _                  _   = Nothing

-- Desugar
desugar :: SCFWAE -> CFWAE
desugar (SID i)     = ID i
desugar (SNum n)    = Num n
desugar (SBool b)    = Bool b
desugar (SOp f exprs) = Op f $ map desugar exprs
desugar (SIf ifTest ifThen ifElse) = If (desugar ifTest) (desugar ifThen) (desugar ifElse)
desugar (SCond cases elseExpr) = desugar $ condToIf cases elseExpr
desugar (SWith bs body)  = desugar $ withToApp bs body
desugar (SMWith bs body) = desugar $ mwithToWith bs body
desugar (SFun ps e)   = desugarFun ps (desugar e)
desugar (SApp e args) = desugarApp (desugar e) (map desugar args)

condToIf :: [Condition] -> SCFWAE -> SCFWAE
condToIf bs elseExpr = foldr (\b -> SIf (caseTest b) (caseThen b)) elseExpr bs

withToApp :: [SBinding] -> SCFWAE -> SCFWAE
withToApp bs body = let (ids, vals) = unzip bs
                    in SApp (SFun ids body) vals

mwithToWith :: [SBinding] -> SCFWAE -> SCFWAE
mwithToWith bs body = foldr (\b -> SWith [b]) body bs

desugarFun :: [String] -> CFWAE -> CFWAE
desugarFun ps body = foldr Fun body ps

desugarApp :: CFWAE -> [CFWAE] -> CFWAE
desugarApp = foldl App
