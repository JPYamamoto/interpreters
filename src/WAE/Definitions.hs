module WAE.Definitions where

type Number = Double

-- Language Definition
data WAE = ID   {name :: String}
         | Num  {value :: Number}
         | Add  {left :: WAE, right:: WAE}
         | Sub  {left :: WAE, right:: WAE}
         | With {idName :: String, idValue :: WAE, body :: WAE}

instance Show WAE where
  show (ID v)    = v
  show (Num n)   = show n
  show (Add l r) = "{+ " ++ show l ++ " " ++ show r ++ "}"
  show (Sub l r) = "{- " ++ show l ++ " " ++ show r ++ "}"
  show (With wID wVal wBody) = concat ["{with {", wID, " ", show wVal, "} ", show wBody, "}"]
