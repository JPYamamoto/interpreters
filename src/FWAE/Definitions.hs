module FWAE.Definitions where

type Number = Double

-- Language Definition
data FWAE = ID   {name :: String}
          | Num  {value :: Number}
          | Add  {left :: FWAE, right:: FWAE}
          | Sub  {left :: FWAE, right:: FWAE}
          | With {idName :: String, idValue :: FWAE, body :: FWAE}
          | Fun  {param :: String, expr :: FWAE}
          | App  {expr1 :: FWAE, expr2 :: FWAE}

instance Show FWAE where
  show (ID v) = v
  show (Num n) = show n
  show (Add l r) = "{+ " ++ show l ++ " " ++ show r ++ "}"
  show (Sub l r) = "{- " ++ show l ++ " " ++ show r ++ "}"
  show (With wID wVal wBody) = concat ["{with {", wID, " ", show wVal, "} ", show wBody, "}"]
  show (Fun p e) = concat ["{fun (", p, ") ", show e, "}"]
  show (App e1 e2) = concat ["app {", show e1, " ", show e2, "}"]
