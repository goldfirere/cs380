-- A parser for the Arith types

module Parser where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Arith

type Parser = Parsec String ()

addOp :: Parser (Sum -> Sum -> Sum)
addOp = plus <|> minus
         where plus  = char '+' >> return Plus
               minus = char '-' >> return Minus

mulOp :: Parser (Term -> Term -> Term)
mulOp = times <|> divide
         where times  = char '*' >> return Mult
               divide = char '/' >> return Div

parenP :: Parser b -> Parser b
parenP p = do char '('
              x <- p
              char ')'
              return x

sumE :: Parser Sum
sumE    = (Term <$> l prodE) `chainl1` l addOp

prodE :: Parser Term
prodE   = (Factor <$> l factorE) `chainl1` l mulOp

factorE :: Parser Factor
factorE = (Sum <$> l (parenP (l sumE))) <|> (Lit <$> l int)

-- consume whitespace after running parser
l :: Parser a -> Parser a
l = lexeme haskell

int :: Parser Integer
int = integer haskell

parseSum :: String -> Sum
parseSum s = case parse (whiteSpace haskell >> sumE) "" s of
  Left err -> error (show err)
  Right x  -> x
