-- Arith.hs
-- Defines algebraic datatypes for storing numerical arithmetic expressions
-- Scroll down to find "Show" instances that might need to be uncommented

{-# LANGUAGE GADTSyntax, StandaloneDeriving #-}

module Arith where

data Sum where
  Plus  :: Sum -> Sum -> Sum
  Minus :: Sum -> Sum -> Sum
  Term  :: Term -> Sum

data Term where
  Mult   :: Term -> Term -> Term
  Div    :: Term -> Term -> Term
  Factor :: Factor -> Term

data Factor where
  Lit :: Integer -> Factor
  Var :: Factor            -- always "x"
  Sum :: Sum -> Factor

-- an Equation has a left-hand side and a right-hand side
data Equation where
  Equation :: Sum -> Sum -> Equation

-- You may wish to uncomment one set of Show instances below,
-- depending on whether you want pretty-printing or ugly-printing.

{-
-- These instances tell GHCi to pretty-print the types above
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
  show Var     = "x"
  show (Sum s) = "(" ++ show s ++ ")"

instance Show Equation where
  show (Equation lhs rhs) = show lhs ++ " = " ++ show rhs
-}

{-
-- These instances tell GHCi to print out the internal structure
-- of the types above

deriving instance Show Sum
deriving instance Show Term
deriving instance Show Factor
deriving instance Show Equation
-}

