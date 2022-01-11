module CFWAE.Definitions where

import Data.Fixed (mod')
import Data.Either

-- Static Scope
type Env = [(String, Value)]
data Value = VNum  {vnumValue :: Number}
           | VBool {vboolValue :: Bool}
           | VClosure {vclosureValue :: Closure} deriving Eq
data Closure = Closure {cparam :: String, cbody :: CFWAE, cenv :: Env} deriving Eq

isVNum :: Value -> Bool
isVNum (VNum _) = True
isVNum _        = False

isVBool :: Value -> Bool
isVBool (VBool _) = True
isVBool _         = False

instance Show Value where
  show (VNum n)     = show n
  show (VBool b)    = show b
  show (VClosure c) = show c

instance Show Closure where
  show (Closure p b _) = show $ Fun p b

type Number = Double

-- Operators
data ArithOp = Add | Sub | Mul | Div | Mod | Expt | Add1 | Sub1 deriving Eq
data RelationalOp = LessT | LessEqT | Equal | GreatT | GreatEqT | IsZero deriving Eq
data LogicalOp  =  Not | And | Or deriving Eq

data Operator = Arithmetic ArithOp | Relational RelationalOp | Logical LogicalOp deriving Eq

instance Show ArithOp where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show Div  = "/"
  show Mod  = "mod"
  show Expt = "expt"
  show Add1 = "add1"
  show Sub1 = "sub1"

instance Show RelationalOp where
  show LessT    = "<"
  show LessEqT  = "<="
  show Equal    = "="
  show GreatT   = ">"
  show GreatEqT = ">="
  show IsZero   = "zero?"

instance Show LogicalOp where
  show Not = "not"
  show And = "and"
  show Or  = "or"

instance Show Operator where
  show (Arithmetic x) = show x
  show (Logical    x) = show x
  show (Relational x) = show x

arithOperatorFun :: ArithOp -> [Number] -> Number
arithOperatorFun Add  xs     = sum xs
arithOperatorFun Sub  xs     = foldl1 (-) xs
arithOperatorFun Mul  xs     = product xs
arithOperatorFun Div  xs     = foldl1 (/) xs
arithOperatorFun Mod  [x, y] = x `mod'` y
arithOperatorFun Expt [x, y] =  x ** y
arithOperatorFun Add1 [x]    = x+1
arithOperatorFun Sub1 [x]    = x-1
arithOperatorFun _    _      = error "Invalid Operation"

logicalOperatorFun :: LogicalOp -> [Bool] -> Bool
logicalOperatorFun Not [x] = not x
logicalOperatorFun And xs  = and xs
logicalOperatorFun Or  xs  = or xs
logicalOperatorFun _   _   = error "Invalid Operation"

relationalOperatorFun :: RelationalOp -> [Number] -> Bool
relationalOperatorFun LessT    xs  = all (uncurry (<))  $ zip xs (tail xs)
relationalOperatorFun LessEqT  xs  = all (uncurry (<=)) $ zip xs (tail xs)
relationalOperatorFun Equal    xs  = all (uncurry (==)) $ zip xs (tail xs)
relationalOperatorFun GreatT   xs  = all (uncurry (>))  $ zip xs (tail xs)
relationalOperatorFun GreatEqT xs  = all (uncurry (>=)) $ zip xs (tail xs)
relationalOperatorFun IsZero   [x] = x == 0
relationalOperatorFun _        _   = error "Invalid Operation"

-- Language Definition
type BaseBinding a = (String, a)
type SBinding = BaseBinding SCFWAE
type Binding = BaseBinding CFWAE

data Condition = Case {caseTest :: SCFWAE, caseThen :: SCFWAE} deriving Eq

-- Version with syntactic sugar
data SCFWAE = SID    {sIDName :: String}
            | SNum   {sNumValue :: Number}
            | SBool  {sBoolValue :: Bool}
            | SOp    {sOpOperator :: Operator, sOpExprs :: [SCFWAE]}
            | SIf    {sIfTest :: SCFWAE, sIfThen :: SCFWAE, sIfElse :: SCFWAE}
            | SCond  {sCondCases :: [Condition], sCondElse :: SCFWAE}
            | SWith  {sWithBindings :: [SBinding], sWithBody :: SCFWAE}
            | SMWith {sMWithBindings :: [SBinding], sMWithBody :: SCFWAE}
            | SFun   {sFunParams :: [String], sFunBody :: SCFWAE}
            | SApp   {sAppLeft :: SCFWAE, sAppRight :: [SCFWAE]}
           deriving Eq

data CFWAE = ID    {idName :: String}
           | Num   {numValue :: Number}
           | Bool  {boolValue :: Bool}
           | Op    {opOperator :: Operator, opExprs :: [CFWAE]}
           | If    {ifTest :: CFWAE, ifThen :: CFWAE, ifElse :: CFWAE}
           | Fun   {funParam :: String, funBody :: CFWAE}
           | App   {appExpr :: CFWAE, appArg :: CFWAE}
           deriving Eq

instance Show SCFWAE where
  show (SID v)       = v
  show (SNum n)      = show n
  show (SBool b)     = show b
  show (SOp op exprs)    = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (SIf c t e)       = "{if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ "}"
  show (SCond cs e)      = "{cond " ++ casesShow cs ++ " " ++ show e ++ "}"
  show (SWith bs wBody)  = concat ["{with {", bindsShow bs, "} ", show wBody, "}"]
  show (SMWith bs wBody) = concat ["{with* {", bindsShow bs, "} ", show wBody, "}"]
  show (SFun ps e)    = concat ["{fun {", unwords ps, "} ", show e, "}"]
  show (SApp e args)  = concat ["{", expressions, "}"]
    where expressions = unwords $ show e : map show args

instance Show CFWAE where
  show (ID v)      = v
  show (Num n)     = show n
  show (Bool b)    = show b
  show (Op op exprs) = "{" ++ show op ++ " " ++ unwords (map show exprs) ++ "}"
  show (If c t e)  = "{if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ "}"
  show (Fun p e)   = concat ["{fun {", p, "} ", show e, "}"]
  show (App e arg) = concat ["{", show e, " ", show arg, "}"]

bindsShow :: Show a => [BaseBinding a] -> String
bindsShow = unwords . map (\(b, e) -> concat ["{", b, " ", show e, "}"])

casesShow :: [Condition] -> String
casesShow = unwords . map (\(Case c t) -> concat ["{", show c, " ", show t, "}"])
