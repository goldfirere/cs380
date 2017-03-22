{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++), last, replicate, length)
import Data.Type.Equality
import Unsafe.Coerce

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- can't do this yet. Need existenials: toSing :: Nat -> SNat n

replicate :: SNat n -> a -> Vec n a
replicate SZero     _ = Nil
replicate (SSucc p) x = x :> replicate p x

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

type family BoolToNat b where
  BoolToNat False = Zero
  BoolToNat True  = Succ Zero

maybeMakeOne :: SBool b -> a -> Vec (BoolToNat b) a
maybeMakeOne SFalse _ = Nil
maybeMakeOne STrue  x = x :> Nil

null :: Vec n a -> Bool
null Nil = True
null _   = False

length :: Vec n a -> SNat n
length Nil       = SZero
length (_ :> xs) = SSucc (length xs)

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

reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go :: forall m p a. Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil     = case (plus_zero acc) of Refl -> acc
    go acc (y:>ys) = case (plus_succ acc ys) of Refl -> go (y :> acc) ys

plus_zero :: forall m a. Vec m a -> m + Zero :~: m  -- :~: comes from Data.Type.Equality
plus_zero Nil = Refl
plus_zero (_ :> xs) = case plus_zero xs of Refl -> Refl
  -- WTP: (Succ m' + Zero) :~: Succ m'
{-# RULES
"plus_zero" forall v. plus_zero v = unsafeCoerce Refl
"plus_succ" forall v1 v2. plus_succ v1 v2 = unsafeCoerce Refl
#-}

plus_succ :: forall m n a. Vec m a -> Vec n a -> (m + Succ n) :~: Succ (m + n)
plus_succ Nil       _  = Refl
plus_succ (_ :> xs) ys = case plus_succ xs ys of Refl -> Refl

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

{-
data a :~: b where
  Refl :: a :~: a
-}
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ v2 = v2
(x :> xs) ++ v2 = x :> (xs ++ v2)

-- map :: (a -> b) -> Vec n a -> Vec n b

last :: Vec (Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs@(_ :> _)) = last xs

-- singleton Nat
{-
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
-}


f :: (a :~: Int) -> a
f Refl = 5

plus_right_id :: SNat n -> (n + Zero) :~: n
plus_right_id SZero = Refl
plus_right_id (SSucc n')
  = case plus_right_id n' of
      Refl -> Refl

-- plus_ri
