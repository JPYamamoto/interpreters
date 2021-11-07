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

integer :: Parser String
integer = positive <|> negative <|> digits
  where digits = many1 digit
        positive = char '+' *> digits
        negative = (:) <$> char '-' <*> digits

float :: Parser String
float = (++) <$> integer <*> decimal
  where decimal = option "" $ (:) <$> char '.' <*> integer

num :: Parser FWAE
num = float >>= (return . Num . read)

add :: Parser FWAE
add = do
  char '+'
  e1 <- expression
  Add e1 <$> expression

sub :: Parser FWAE
sub = do
  char '-'
  e1 <- expression
  Sub e1 <$> expression

with :: Parser FWAE
with = try $ do
  string "with"
  spaces
  (wvar, wval) <- parenthesis bind
  spaces
  With wvar wval <$> expression

bind :: Parser (String, FWAE)
bind = do
  wvar <- word
  spaces
  wval <- expression
  return (wvar, wval)

fun :: Parser FWAE
fun = try $ do
  string "fun"
  spaces
  fparam <- parenthesis word
  spaces
  Fun fparam <$> expression

app :: Parser FWAE
app = do
  e1 <- expression
  spaces
  App e1 <$> expression

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
