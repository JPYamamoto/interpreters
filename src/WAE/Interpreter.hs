module WAE.Interpreter (interp) where

import WAE.Definitions

-- Semantic Analysis
subst :: WAE -> String -> WAE -> WAE
subst (ID v)   subID val = if subID == v then val else (ID v)
subst n@(Num _) _     _   = n
subst (Add l r) subID val = Add (subst l subID val) (subst r subID val)
subst (Sub l r) subID val = Sub (subst l subID val) (subst r subID val)
subst (With wname val wbody) subId subVal
  | wname == subId = With wname (subst val subId subVal) wbody
  | otherwise      = With wname (subst val subId subVal) (subst wbody subId subVal)

interp :: WAE -> Number
interp (ID _) = error "Unbound identifier."
interp (Num n) = n
interp (Add l r) = (interp l) + (interp r)
interp (Sub l r) = (interp l) - (interp r)
interp (With wID wVal wBody) = interp (subst wBody wID wVal)
