-- Peano.hs
-- A definition of Peano natural numbers

{-# LANGUAGE GADTSyntax #-}

module Peano where

-- All natural numbers are either 0 or a successor of a natural number.
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

-- GHCi should print Nats just like ordinary numbers
instance Show Nat where
  show = show . fromNat  -- NB: This is *not* recursive (why?)

-- Convert an Integer to a Nat:
toNat :: Integer -> Nat
toNat n | n < 0 = error "No negative Nats"
toNat 0         = Zero
toNat n         = Succ (toNat (n-1))

-- Convert a Nat back to an Integer:
fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + (fromNat n)

-- Addition on Nats
plus :: Nat -> Nat -> Nat
plus Zero     m = m
plus (Succ n) m = Succ (plus n m)

-- Multiplication on Nats
mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ n) m = plus m (mult n m)
