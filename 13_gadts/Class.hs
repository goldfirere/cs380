{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++) )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

stuff = 5 :> 3 :> 8 :> Nil

safeHead :: Vec (Succ n) a -> a
safeHead (x :> _) = x
-- NO!  safeHead Nil      = error "urk"

safeTail :: Vec (Succ n) a -> Vec n a
safeTail (_ :> xs) = xs

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil       x = x :> Nil
snoc (y :> ys) x = y :> snoc ys x

reverse :: Vec n a -> Vec n a
reverse Nil       = Nil
reverse (x :> xs) = snoc (reverse xs) x

reverseList :: [a] -> [a]
reverseList xs = go [] xs
  where
    go acc []     = acc
    go acc (y:ys) = go (y:acc) ys
{-
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go :: Vec m a -> Vec p a -> Vec (Succ m) a
    go acc Nil     = acc
    go acc (y:>ys) = go (y :> acc) ys
-}

type family Plus (a :: Nat) (b :: Nat) :: Nat where
  Plus Zero     b = b
  Plus (Succ a) b = Succ (Plus a b)

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ v2 = v2
(x :> xs) ++ v2 = x :> (xs ++ v2)
