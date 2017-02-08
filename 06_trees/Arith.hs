-- Arith.hs
-- Defines algebraic datatypes for storing numerical arithmetic expressions

{-# LANGUAGE GADTSyntax #-}

module Arith where

data Sum where
  Plus  :: Sum -> Sum -> Sum
  Minus :: Sum -> Sum -> Sum
  Term  :: Term -> Sum
  deriving Show

data Term where
  Mult   :: Term -> Term -> Term
  Div    :: Term -> Term -> Term
  Factor :: Factor -> Term
  deriving Show

data Factor where
  Lit :: Integer -> Factor
  Sum :: Sum -> Factor
  deriving Show


evalSum :: Sum -> Integer
evalSum (Plus a b)  = evalSum a + evalSum b
evalSum (Minus a b) = evalSum a - evalSum b
evalSum (Term t)    = evalTerm t

evalTerm :: Term -> Integer
evalTerm (Mult a b) = evalTerm a * evalTerm b
evalTerm (Div a b)  = evalTerm a `div`
                      evalTerm b
evalTerm (Factor f) = evalFactor f

evalFactor :: Factor -> Integer
evalFactor (Lit x) = x
evalFactor (Sum s) = evalSum s

{-
-- These instances tell GHCi to pretty-print the types above
-- Remove the (deriving Show) lines to allow these to work

instance Show Sum where
  show (Plus a b) = show a ++ " + " ++ show b
  show (Minus a b) = show a ++ " - " ++ show b
  show (Term t) = show t

instance Show Term where
  show (Mult a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b
  show (Factor f) = show f

instance Show Factor where
  show (Lit l) = show l
  show (Sum s) = "(" ++ show s ++ ")"
-}
