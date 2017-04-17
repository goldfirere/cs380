-- Parser.hs
-- A parser for arithmetic expressions.
-- You do NOT need to understand the code in this module!
-- This module exports only the three functions at the end,
-- whose types tell you pretty well what they do.

-- If parsing fails, an exception is thrown.

module Parser ( parseSum, parseTerm, parseFactor ) where

import Prelude hiding ( sum )
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Arith
import Nat

type Parser = Parsec String ()

-- Use the haskell Parsec language definition to aid lexing

-- consume whitespace after running parser
l :: Parser a -> Parser a
l = lexeme haskell

int :: Parser Integer
int = integer haskell

ws :: Parser ()
ws = whiteSpace haskell

addOp :: Parser (Sum n -> Sum n -> Sum n)
addOp = plus <|> minus
  where plus  = Plus  <$ char '+'
        minus = Minus <$ char '-'

mulOp :: Parser (Term n -> Term n -> Term n)
mulOp = times <|> divide
  where times  = Mult <$ char '*'
        divide = Div  <$ char '/'

parenP :: Parser b -> Parser b
parenP p = char '(' *> p <* char ')'

sum :: SNat n -> Parser (Sum n)
sum n = (Term <$> l (term n)) `chainl1` l addOp

term :: SNat n -> Parser (Term n)
term n = (Factor <$> l (factor n)) `chainl1` l mulOp

factor :: SNat n -> Parser (Factor n)
factor n = (Var <$> l (fin n)) <|>
           (Sum <$> l (parenP (l (sum n)))) <|>
           (Lit <$> l int)

fin :: SNat n -> Parser (Fin n)
fin n = do
  char 'x'
  x <- int
  toFin n x

-- convenient wrapper
doParse :: Parser a -> String -> a
doParse p s = case parse (ws *> p <* eof) "" s of
  Left err -> error (show err)
  Right x  -> x

parseSum :: SNat n -> String -> Sum n
parseSum n = doParse (sum n)

parseTerm :: SNat n -> String -> Term n
parseTerm n = doParse (term n)

parseFactor :: SNat n -> String -> Factor n
parseFactor n = doParse (factor n)
