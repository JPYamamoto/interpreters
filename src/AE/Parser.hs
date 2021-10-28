module AE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import AE.Definitions

num :: Parser AE
num = do
  n <- many1 digit
  return $ Num (read n :: Double)

add :: Parser AE
add = do
  char '+'
  e1 <- expression
  e2 <- expression
  return $ Add e1 e2

sub :: Parser AE
sub = do
  char '-'
  e1 <- expression
  e2 <- expression
  return $ Sub e1 e2

parenthesis :: Parser AE -> Parser AE
parenthesis p = do
  char '{'
  e <- p
  char '}'
  return e

expression :: Parser AE
expression = spaces *> (num <|> parenthesis recursiveExpression) <* spaces

recursiveExpression :: Parser AE
recursiveExpression = add <|> sub

aeProgram :: Parser AE
aeProgram = do
  e <- expression
  eof
  return e

parse :: String -> Either ParseError AE
parse = P.parse aeProgram ""
