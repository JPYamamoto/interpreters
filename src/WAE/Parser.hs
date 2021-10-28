module WAE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import WAE.Definitions

idVar :: Parser WAE
idVar = do
  word <- many1 letter
  return $ ID word

num :: Parser WAE
num = do
  n <- many1 digit
  return $ Num (read n :: Double)

add :: Parser WAE
add = do
  char '+'
  e1 <- expression
  e2 <- expression
  return $ Add e1 e2

sub :: Parser WAE
sub = do
  char '-'
  e1 <- expression
  e2 <- expression
  return $ Sub e1 e2

with :: Parser WAE
with = do
  string "with"
  spaces
  (wvar, wval) <- bind
  spaces
  wbody <- expression
  return $ With wvar wval wbody

bind :: Parser (String, WAE)
bind = do
  char '{'
  spaces
  wvar <- many1 letter
  spaces
  wval <- expression
  spaces
  char '}'
  return $ (wvar, wval)

parenthesis :: Parser WAE -> Parser WAE
parenthesis p = do
  char '{'
  e <- p
  char '}'
  return e

expression :: Parser WAE
expression = spaces *> (num <|> idVar <|> parenthesis recursiveExpression) <* spaces

recursiveExpression :: Parser WAE
recursiveExpression = with <|> add <|> sub

waeProgram :: Parser WAE
waeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError WAE
parse = P.parse waeProgram ""
