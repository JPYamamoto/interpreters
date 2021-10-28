module FWAE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import FWAE.Definitions

word :: Parser String
word = many1 letter

idVar :: Parser FWAE
idVar = do
  varname <- word
  return $ ID varname

num :: Parser FWAE
num = do
  n <- many1 digit
  return $ Num (read n :: Double)

add :: Parser FWAE
add = do
  char '+'
  e1 <- expression
  e2 <- expression
  return $ Add e1 e2

sub :: Parser FWAE
sub = do
  char '-'
  e1 <- expression
  e2 <- expression
  return $ Sub e1 e2

with :: Parser FWAE
with = try $ do
  string "with"
  spaces
  (wvar, wval) <- parenthesis bind
  spaces
  wbody <- expression
  return $ With wvar wval wbody

bind :: Parser (String, FWAE)
bind = do
  wvar <- word
  spaces
  wval <- expression
  return $ (wvar, wval)

fun :: Parser FWAE
fun = try $ do
  string "fun"
  spaces
  fparam <- parenthesis word
  spaces
  fbody <- expression
  return $ Fun fparam fbody

app :: Parser FWAE
app = do
  e1 <- expression
  spaces
  e2 <- expression
  return $ App e1 e2

parenthesis :: Parser a -> Parser a
parenthesis p = do
  char '{'
  spaces
  e <- p
  spaces
  char '}'
  return e

expression :: Parser FWAE
expression = spaces *> expressions <* spaces
  where expressions = parenthesis recursiveExpression <|> num <|> idVar

recursiveExpression :: Parser FWAE
recursiveExpression = add <|> sub <|> with <|> fun <|> app

fwaeProgram :: Parser FWAE
fwaeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError FWAE
parse = P.parse fwaeProgram ""
