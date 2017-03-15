{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++), last )
import Data.Type.Equality

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
reverseVec xs = go SZero Nil xs
  where
    go :: SNat m -> Vec m a -> Vec p a -> Vec (m + p) a
    go len acc Nil     = case plus_right_id len of Refl -> acc
    go len acc (y:>ys) = go (SSucc len) (y :> acc) ys
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

-- map :: (a -> b) -> Vec n a -> Vec n b

last :: Vec (Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs@(_ :> _)) = last xs

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

{-
data a :~: b where
  Refl :: a :~: a
-}

f :: (a :~: Int) -> a
f Refl = 5

plus_right_id :: SNat n -> (n + Zero) :~: n
plus_right_id SZero = Refl
plus_right_id (SSucc n')
  = case plus_right_id n' of
      Refl -> Refl

-- plus_ri
