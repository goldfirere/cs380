{-# LANGUAGE GADTSyntax #-}

module Class where

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

{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)
  {-# MINIMAL (==) | (/=) #-}
-}
{-
instance Eq Nat where
  Zero   == Zero     = True
  Succ n == Succ m   = n == m
  _      == _        = False
-}
{-
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing    = True
  Just x  == Just y     = x == y
  _       == _          = False

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _      == _      = False
-}

instance Ord Nat where
  Zero   <= _      = True
  Succ n <= Succ m = n <= m
  _      <= _      = False

f :: Ord a => a -> a -> Bool
f x y = x == y

instance Num Nat where
  a + b = plus a b

  a      - Zero   = a
  Succ a - Succ b = a - b
  _      - _      = error "no negative Nats"

  a * b = mult a b

  negate Zero = Zero
  negate _    = error "no negative Nats"

  abs a = a

  signum Zero     = Zero
  signum (Succ _) = Succ Zero

  fromInteger a = toNat a
