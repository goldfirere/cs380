-- Arith.hs
-- Defines algebraic datatypes for storing numerical arithmetic expressions

{-# LANGUAGE GADTs, StandaloneDeriving, TypeInType #-}

module Arith where

import Data.Kind ( Type )

import NatVec

-- All types are indexed by the number of variables that can occur in the expr

data Sum :: Nat -> Type where
  Plus  :: Sum n -> Sum n -> Sum n
  Minus :: Sum n -> Sum n -> Sum n
  Term  :: Term n -> Sum n

data Term :: Nat -> Type where
  Mult   :: Term n -> Term n -> Term n
  Div    :: Term n -> Term n -> Term n
  Factor :: Factor n -> Term n

data Factor :: Nat -> Type where
  Lit :: Integer -> Factor n
  Var :: Fin n -> Factor n
  Sum :: Sum n -> Factor n

deriving instance Eq (Sum n)
deriving instance Eq (Term n)
deriving instance Eq (Factor n)

-- These instances tell GHCi to pretty-print the types above
instance Show (Sum n) where
  show (Plus a b)  = show a ++ " + " ++ show b
  show (Minus a b) = show a ++ " - " ++ show b
  show (Term t)    = show t

instance Show (Term n) where
  show (Mult a b) = show a ++ " * " ++ show b
  show (Div a b)  = show a ++ " / " ++ show b
  show (Factor f) = show f

instance Show (Factor n) where
  show (Lit l) = show l
  show (Var n) = "x" ++ show n
  show (Sum s) = "(" ++ show s ++ ")"
