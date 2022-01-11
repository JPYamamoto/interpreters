module CFWAE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import CFWAE.Definitions

-- Primitives

idVar :: Parser SCFWAE
idVar = do
  varname <- word
  return $ SID varname

integer :: Parser String
integer = positive <|> negative <|> digits
  where digits = many1 digit
        positive = char '+' *> digits
        negative = (:) <$> char '-' <*> digits

float :: Parser String
float = (++) <$> integer <*> decimal
  where decimal = option "" $ (:) <$> char '.' <*> integer

num :: Parser SCFWAE
num = float >>= (return . SNum . read)

boolean :: Parser SCFWAE
boolean = try (true <|> false)

true :: Parser SCFWAE
true = do
  string "true"
  return (SBool True)

false :: Parser SCFWAE
false = do
  string "false"
  return (SBool False)


-- Operators

add :: Parser SCFWAE
add = do
  char '+'
  exps <- many1 expression
  return $ SOp (Arithmetic Add) exps

sub :: Parser SCFWAE
sub = do
  char '-'
  exps <- many1 expression
  return $ SOp (Arithmetic Sub) exps

mul :: Parser SCFWAE
mul = do
  char '*'
  exps <- many1 expression
  return $ SOp (Arithmetic Mul) exps

divOp :: Parser SCFWAE
divOp = do
  char '/'
  exps <- many1 expression
  return $ SOp (Arithmetic Div) exps

modOp :: Parser SCFWAE
modOp = do
  string "modulo"
  e1 <- expression
  e2 <- expression
  return $ SOp (Arithmetic Mod) [e1, e2]

expt :: Parser SCFWAE
expt = do
  string "expt"
  e1 <- expression
  e2 <- expression
  return $ SOp (Arithmetic Expt) [e1, e2]

add1 :: Parser SCFWAE
add1 = do
  string "add1"
  e1 <- expression
  return $ SOp (Arithmetic Add1) [e1]

sub1 :: Parser SCFWAE
sub1 = do
  string "sub1"
  e1 <- expression
  return $ SOp (Arithmetic Sub1) [e1]

lessTOp :: Parser SCFWAE
lessTOp = do
  char '<'
  exps <- many1 expression
  return $ SOp (Relational LessT) exps

lessEqTOp :: Parser SCFWAE
lessEqTOp = do
  string "<="
  exps <- many1 expression
  return $ SOp (Relational LessEqT) exps

eqOp :: Parser SCFWAE
eqOp = do
  char '='
  exps <- many1 expression
  return $ SOp (Relational Equal) exps

greatTOp :: Parser SCFWAE
greatTOp = do
  char '>'
  exps <- many1 expression
  return $ SOp (Relational GreatT) exps

greatEqTOp :: Parser SCFWAE
greatEqTOp = do
  string ">="
  exps <- many1 expression
  return $ SOp (Relational GreatEqT) exps

isZeroOp :: Parser SCFWAE
isZeroOp = do
  string "zero?"
  exp <- expression
  return $ SOp (Relational IsZero) [exp]

notOp :: Parser SCFWAE
notOp = do
  string "not"
  exp <- expression
  return $ SOp (Logical Not) [exp]

andOp :: Parser SCFWAE
andOp = do
  string "and"
  exps <- many1 expression
  return $ SOp (Logical And) exps

orOp :: Parser SCFWAE
orOp = do
  string "or"
  exps <- many1 expression
  return $ SOp (Logical Or) exps


-- With/Functions Expressions

condition :: Parser SCFWAE
condition = try $ do
  string "cond"
  spaces
  cases <- sepEndBy1 (parenthesis conditionCase) spaces
  if caseTest (last cases) /= SID "else"
    then fail "Missing else in condition."
    else return $ SCond (init cases) (caseThen $ last cases)

conditionCase :: Parser Condition
conditionCase = try $ do
  testExpr <- expression
  spaces
  thenExpr <- expression
  spaces
  return $ Case testExpr thenExpr

ifParse :: Parser SCFWAE
ifParse = try $ do
  string "if"
  testExpr <- expression
  thenExpr <- expression
  elseExpr <- expression
  return $ SIf testExpr thenExpr elseExpr

with :: Parser SCFWAE
with = try $ do
  string "with"
  spaces
  bindings <- parenthesis $ sepBy1 bind spaces
  spaces
  wbody <- expression
  return $ SWith bindings wbody

withM :: Parser SCFWAE
withM = try $ do
  string "with*"
  spaces
  bindings <- parenthesis $ sepBy1 bind spaces
  spaces
  wbody <- expression
  return $ SMWith bindings wbody

bind :: Parser SBinding
bind = do
  char '{'
  spaces
  wvar <- many1 letter
  spaces
  wval <- expression
  spaces
  char '}'
  return (wvar, wval)

fun :: Parser SCFWAE
fun = try $ do
  string "fun"
  spaces
  fparams <- parenthesis $ sepBy1 word spaces
  spaces
  SFun fparams <$> expression

app :: Parser SCFWAE
app = try $ do
  e <- expression
  spaces
  es <- many1 expression
  return $ SApp e es

-- Utils

parenthesis :: Parser a -> Parser a
parenthesis p = do
  char '{'
  spaces
  e <- p
  spaces
  char '}'
  return e

word :: Parser String
word = many1 letter


-- Program

expression :: Parser SCFWAE
expression = spaces *> expressions <* spaces
  where expressions = parenthesis recursiveExpression <|> num <|> boolean <|> idVar

recursiveExpression :: Parser SCFWAE
recursiveExpression = operators <|> condition <|> ifParse <|> with <|> withM <|> fun <|> app

operators :: Parser SCFWAE
operators = choice . map try $ arithOp ++ relationalOp ++ logicalOp
  where arithOp = [add, sub, mul, divOp, modOp, expt, add1, sub1]
        relationalOp = [lessTOp, lessEqTOp, eqOp, greatTOp, greatEqTOp, isZeroOp]
        logicalOp = [notOp, andOp, orOp]

fwaeProgram :: Parser SCFWAE
fwaeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError SCFWAE
parse s = P.parse fwaeProgram "" cleanS
  where cleanS = map (\c -> if c == '\n' then ' ' else c) s
