module AE.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

import AE.Definitions

integer :: Parser String
integer = positive <|> negative <|> digits
  where digits = many1 digit
        positive = char '+' *> digits
        negative = (:) <$> char '-' <*> digits

float :: Parser String
float = (++) <$> integer <*> decimal
  where decimal = option "" $ (:) <$> char '.' <*> integer

num :: Parser AE
num = float >>= (return . Num . read)

add :: Parser AE
add = do
  char '+'
  e1 <- expression
  Add e1 <$> expression

sub :: Parser AE
sub = do
  char '-'
  e1 <- expression
  Sub e1 <$> expression

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
