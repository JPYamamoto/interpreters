module WAE.Interpreter (interp) where

import WAE.Definitions

-- Semantic Analysis

-- Substitution
subst :: WAE -> String -> WAE -> WAE
subst (ID v)   subID val     = if subID == v then val else ID v
subst n@(Num _) _     _      = n
subst (Op op exps) subID val = Op op (map (\e -> subst e subID val) exps)
subst (With wbindings wbody) subID subVal
  | inBindings subID wbindings = With (map (\b -> substBinding b subID subVal) wbindings) wbody
  | otherwise = With (map (\b -> substBinding b subID subVal) wbindings) (subst wbody subID subVal)

inBindings :: String -> [(String, WAE)] -> Bool
inBindings bSearch = any (\(bID, _) -> bID == bSearch)

substBinding :: (String, WAE) -> String -> WAE -> (String, WAE)
substBinding (bID, bExpr) subID subVal = (bID, subst bExpr subID subVal)

-- Interpreter
interp :: SWAE -> Number
interp = interp' . desugar

interp' :: WAE -> Number
interp' (ID _)                 = error "Unbound identifier."
interp' (Num n)                = n
interp' (Op op exps)           = operatorFun op $ map interp' exps
interp' (With wBindings wBody) = interp' $ applyBindings wBindings wBody

applyBindings :: [Binding] -> WAE -> WAE
applyBindings [] body               = body
applyBindings ((bID, bVal):bs) body = applyBindings bs $ subst body bID (Num $ interp' bVal)

-- Desugar
desugar :: SWAE -> WAE
desugar (SID x)         = ID x
desugar (SNum x)        = Num x
desugar (SOp f exprs)   = Op f $ map desugar exprs
desugar (SWith bs body) = With (map desugarBinding bs) (desugar body)
desugar mwith           = desugar $ mwithToWith mwith

mwithToWith :: SWAE -> SWAE
mwithToWith (SMWith [] mwbody)     = mwbody
mwithToWith (SMWith (b:bs) mwbody) = SWith [b] $ mwithToWith (SMWith bs mwbody)
mwithToWith _                      = error "Invalid with*"

desugarBinding :: SBinding -> Binding
desugarBinding (name, value) = (name, desugar value)
