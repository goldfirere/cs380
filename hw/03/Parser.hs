-- Parser.hs
-- A parser for arithmetic expressions.
-- You do NOT need to understand the code in this module!
-- This module exports only the four functions at the end,
-- whose types tell you pretty well what they do.

-- If parsing fails, an exception is thrown.

module Parser ( parseSum, parseTerm, parseFactor, parseEquation ) where

import Prelude hiding ( sum )
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Arith

type Parser = Parsec String ()

-- Use the haskell Parsec language definition to aid lexing

-- consume whitespace after running parser
l :: Parser a -> Parser a
l = lexeme haskell

int :: Parser Integer
int = integer haskell

ws :: Parser ()
ws = whiteSpace haskell

addOp :: Parser (Sum -> Sum -> Sum)
addOp = plus <|> minus
  where plus  = Plus  <$ char '+'
        minus = Minus <$ char '-'

mulOp :: Parser (Term -> Term -> Term)
mulOp = times <|> divide
  where times  = Mult <$ char '*'
        divide = Div  <$ char '/'

parenP :: Parser b -> Parser b
parenP p = char '(' *> p <* char ')'

sum :: Parser Sum
sum = (Term <$> l term) `chainl1` l addOp

term :: Parser Term
term = (Factor <$> l factor) `chainl1` l mulOp

factor :: Parser Factor
factor = (Var <$ l (char 'x')) <|>
         (Sum <$> l (parenP (l sum))) <|>
         (Lit <$> l int)

equation :: Parser Equation
equation = Equation <$> sum <* l (char '=') <*> sum

-- convenient wrapper
doParse :: Parser a -> String -> a
doParse p s = case parse (ws *> p <* eof) "" s of
  Left err -> error (show err)
  Right x  -> x

parseSum :: String -> Sum
parseSum = doParse sum

parseTerm :: String -> Term
parseTerm = doParse term

parseFactor :: String -> Factor
parseFactor = doParse factor

parseEquation :: String -> Equation
parseEquation = doParse equation

