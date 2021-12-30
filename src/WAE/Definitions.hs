module WAE.Definitions where

import Data.Fixed (mod')

type Number = Double

-- Operators
data ArithOp = Add | Sub | Mul | Div | Mod | Expt | Add1 | Sub1

instance Show ArithOp where
  show Add = show "+"
  show Sub = show "-"
  show Mul = show "*"
  show Div = show "/"
  show Mod = show "mod"
  show Expt = show "expt"
  show Add1 = show "add1"
  show Sub1 = show "sub1"

operatorFun :: ArithOp -> ([Number] -> Number)
operatorFun Add  xs     = sum xs
operatorFun Sub  xs     = foldl1 (-) xs
operatorFun Mul  xs     = product xs
operatorFun Div  xs     = foldl1 (/) xs
operatorFun Mod  [x, y] = x `mod'` y
operatorFun Expt [x, y] =  x ** y
operatorFun Add1 [x]    = x+1
operatorFun Sub1 [x]    = x-1
operatorFun _    _      = error "Invalid Operation"

-- Language Definition
type BaseBinding a = (String, a)
type SBinding = BaseBinding SWAE
type Binding = BaseBinding WAE

-- Syntactic Sugar Version
data SWAE = SID    {sIDName :: String}
          | SNum   {sNumValue :: Number}
          | SOp    {sOpOperator :: ArithOp, sOpExprs :: [SWAE]}
          | SWith  {sWithBindings :: [SBinding], sWithBody :: SWAE}
          | SMWith {sMWithBindings :: [SBinding], sMWithBody :: SWAE}

data WAE = ID   {idName :: String}
         | Num  {numValue :: Number}
         | Op   {opOperator :: ArithOp, opExprs :: [WAE]}
         | With {withBindings :: [Binding], withBody :: WAE}

instance Show SWAE where
  show (SID v)           = v
  show (SNum n)          = show n
  show (SOp op exprs)    = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (SWith bs wBody)  = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]
  show (SMWith bs wBody) = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]

instance Show WAE where
  show (ID v)          = v
  show (Num n)         = show n
  show (Op op exprs)   = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (With bs wBody) = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]

bindsShow :: Show a => [BaseBinding a] -> String
bindsShow = unwords . map (\(b, e) -> concat ["{", b, show e, "}"])
