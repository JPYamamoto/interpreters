module WAE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import WAE.Definitions

--Primitives

idVar :: Parser SWAE
idVar = do
  word <- many1 letter
  return $ SID word

integer :: Parser String
integer = positive <|> negative <|> digits
  where digits = many1 digit
        positive = char '+' *> digits
        negative = (:) <$> char '-' <*> digits

float :: Parser String
float = (++) <$> integer <*> decimal
  where decimal = option "" $ (:) <$> char '.' <*> integer

num :: Parser SWAE
num = float >>= (return . SNum . read)


-- Operators

add :: Parser SWAE
add = do
  char '+'
  exps <- many1 expression
  return $ SOp Add exps

sub :: Parser SWAE
sub = do
  char '-'
  exps <- many1 expression
  return $ SOp Sub exps

mul :: Parser SWAE
mul = do
  char '*'
  exps <- many1 expression
  return $ SOp Mul exps

divOp :: Parser SWAE
divOp = do
  char '/'
  exps <- many1 expression
  return $ SOp Div exps

modOp :: Parser SWAE
modOp = do
  string "modulo"
  e1 <- expression
  e2 <- expression
  return $ SOp Mod [e1, e2]

expt :: Parser SWAE
expt = do
  string "expt"
  e1 <- expression
  e2 <- expression
  return $ SOp Expt [e1, e2]

add1 :: Parser SWAE
add1 = do
  string "add1"
  e1 <- expression
  return $ SOp Add1 [e1]

sub1 :: Parser SWAE
sub1 = do
  string "sub1"
  e1 <- expression
  return $ SOp Sub1 [e1]


-- With Expressions

with :: Parser SWAE
with = try $ do
  string "with"
  spaces
  bindings <- parenthesis $ many1 bind
  spaces
  wbody <- expression
  return $ SWith bindings wbody

withM :: Parser SWAE
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


-- Utils

parenthesis :: Parser a -> Parser a
parenthesis p = do
  char '{'
  e <- p
  char '}'
  return e

-- Program

expression :: Parser SWAE
expression = spaces *> expressions <* spaces
  where expressions = parenthesis recursiveExpression <|> num <|> idVar

recursiveExpression :: Parser SWAE
recursiveExpression = operators <|> with <|> withM
  where operators = add <|> sub <|> mul <|> divOp <|> modOp <|> expt <|> add1 <|> sub1

waeProgram :: Parser SWAE
waeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError SWAE
parse = P.parse waeProgram ""
