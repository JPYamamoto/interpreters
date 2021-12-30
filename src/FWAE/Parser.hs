module FWAE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import FWAE.Definitions

-- Primitives

idVar :: Parser SFWAE
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

num :: Parser SFWAE
num = float >>= (return . SNum . read)


-- Operators

add :: Parser SFWAE
add = do
  char '+'
  exps <- many1 expression
  return $ SOp Add exps

sub :: Parser SFWAE
sub = do
  char '-'
  exps <- many1 expression
  return $ SOp Sub exps

mul :: Parser SFWAE
mul = do
  char '*'
  exps <- many1 expression
  return $ SOp Mul exps

divOp :: Parser SFWAE
divOp = do
  char '/'
  exps <- many1 expression
  return $ SOp Div exps

modOp :: Parser SFWAE
modOp = do
  string "modulo"
  e1 <- expression
  e2 <- expression
  return $ SOp Mod [e1, e2]

expt :: Parser SFWAE
expt = do
  string "expt"
  e1 <- expression
  e2 <- expression
  return $ SOp Expt [e1, e2]

add1 :: Parser SFWAE
add1 = do
  string "add1"
  e1 <- expression
  return $ SOp Add1 [e1]

sub1 :: Parser SFWAE
sub1 = do
  string "sub1"
  e1 <- expression
  return $ SOp Sub1 [e1]


-- With/Functions Expressions

with :: Parser SFWAE
with = try $ do
  string "with"
  spaces
  bindings <- parenthesis $ many1 bind
  spaces
  wbody <- expression
  return $ SWith bindings wbody

withM :: Parser SFWAE
withM = try $ do
  string "with*"
  spaces
  bindings <- parenthesis $ many1 bind
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
  return $ (wvar, wval)

fun :: Parser SFWAE
fun = try $ do
  string "fun"
  spaces
  fparams <- parenthesis $ many1 word
  spaces
  SFun fparams <$> expression

app :: Parser SFWAE
app = do
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

expression :: Parser SFWAE
expression = spaces *> expressions <* spaces
  where expressions = parenthesis recursiveExpression <|> num <|> idVar

recursiveExpression :: Parser SFWAE
recursiveExpression = operators <|> with <|> fun <|> app
  where operators = add <|> sub <|> mul <|> divOp <|> modOp <|> expt <|> add1 <|> sub1

fwaeProgram :: Parser SFWAE
fwaeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError SFWAE
parse = P.parse fwaeProgram ""
