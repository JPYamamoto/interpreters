module FWAE.Definitions where

import Data.Fixed (mod')
import Data.Either

-- Static Scope
type FAEResult = Either Closure Number
type Env = [(String, FAEResult)]
data Closure = Closure {cparam :: String, cbody :: FWAE, cenv :: Env}

instance Show Closure where
  show (Closure p b _) = show $ Fun p b

type Number = Double

-- Operators
data ArithOp = Add | Sub | Mul | Div | Mod | Expt | Add1 | Sub1

instance Show ArithOp where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show Div  = "/"
  show Mod  = "mod"
  show Expt = "expt"
  show Add1 = "add1"
  show Sub1 = "sub1"

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
type SBinding = BaseBinding SFWAE
type Binding = BaseBinding FWAE

-- Version with syntactic sugar
data SFWAE = SID    {sIDName :: String}
           | SNum   {sNumValue :: Number}
           | SOp    {sOpOperator :: ArithOp, sOpExprs :: [SFWAE]}
           | SWith  {sWithBindings :: [SBinding], sWithBody :: SFWAE}
           | SMWith {sMWithBindings :: [SBinding], sMWithBody :: SFWAE}
           | SFun   {sFunParams :: [String], sFunBody :: SFWAE}
           | SApp   {sAppLeft :: SFWAE, sAppRight :: [SFWAE]}

data FWAE = ID    {idName :: String}
          | Num   {numValue :: Number}
          | Op    {opOperator :: ArithOp, opExprs :: [FWAE]}
          | Fun   {funParam :: String, funBody :: FWAE}
          | App   {appExpr :: FWAE, appArg :: FWAE}

instance Show SFWAE where
  show (SID v)        = v
  show (SNum n)       = show n
  show (SOp op exprs)    = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (SWith bs wBody)  = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]
  show (SMWith bs wBody) = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]
  show (SFun ps e)    = concat ["{fun {", unwords ps, "} ", show e, "}"]
  show (SApp e args)  = concat ["{", expressions, "}"]
    where expressions = unwords $ show e : map show args

instance Show FWAE where
  show (ID v)      = v
  show (Num n)     = show n
  show (Op op exprs) = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (Fun p e)   = concat ["{fun {", p, "} ", show e, "}"]
  show (App e arg) = concat ["{", show e, " ", show arg, "}"]

bindsShow :: Show a => [BaseBinding a] -> String
bindsShow = unwords . map (\(b, e) -> concat ["{", b, " ", show e, "}"])
