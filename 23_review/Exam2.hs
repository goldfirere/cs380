{-# LANGUAGE ScopedTypeVariables, GADTs, TypeInType, TypeOperators,
             StandaloneDeriving, TypeApplications, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes #-}

module Exam2 where

import Prelude ( Bool(..), Maybe(..), Num(..), otherwise, undefined, error,
                 Eq(..), Ord(..), Show(..), (&&), (||), not, ($), (.),
                 Int, Integer, Double, Float, Char, Functor(..) )


import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)(Refl), type (==) )

-- Bools
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

-- Lists
type family Length (l :: [a]) :: Nat where
  Length '[]      = Zero
  Length (_ : xs) = Succ (Length xs)

-- Naturals
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving (Eq, Show)

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
deriving instance Show (SNat n)

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

type family a - b where
  a - Zero        = a
  Zero - b        = Zero
  Succ a - Succ b = a - b

type family a * b where
  Zero   * b = Zero
  Succ a * b = b + a * b
infixl 7 *

type family Pred n where
  Pred Zero     = Zero
  Pred (Succ n) = n

type family IsZero n where
  IsZero Zero     = True
  IsZero (Succ _) = False

type family Sum ns where
  Sum '[]      = Zero
  Sum (n : ns) = n + Sum ns

type family Min a b where
  Min Zero b = Zero
  Min a Zero = Zero
  Min (Succ a) (Succ b) = Succ (Min a b)

plusZero :: SNat n -> (n + Zero) :~: n
plusZero SZero = Refl
plusZero (SSucc n') = case plusZero n' of Refl -> Refl

plusSucc :: forall m n. SNat n -> (n + Succ m) :~: Succ (n + m)
plusSucc SZero      = Refl
plusSucc (SSucc n') = case plusSucc @m n' of Refl -> Refl

plusAssoc :: forall n p m. SNat m -> (m + n + p) :~: (m + (n + p))
plusAssoc SZero      = Refl
plusAssoc (SSucc m') = case plusAssoc @n @p m' of Refl -> Refl

plusComm :: forall n m. SNat m -> SNat n -> m + n :~: n + m
plusComm SZero                   n = case plusZero n of Refl -> Refl
plusComm (SSucc (m' :: SNat m')) n =
  case plusSucc @m' n of Refl -> case plusComm m' n of Refl -> Refl

-- Fin
data Fin :: Nat -> Type where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)
deriving instance Eq   (Fin n)
deriving instance Show (Fin n)

-- Vecs
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Eq a   => Eq (Vec n a)
deriving instance Show a => Show (Vec n a)

length :: Vec n a -> SNat n
length Nil       = SZero
length (_ :> xs) = SSucc (length xs)

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil = Nil
map f (x :> xs) = f x :> map f xs

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ w = w
(x :> xs) ++ w = x :> (xs ++ w)

reverse :: forall n a. Vec n a -> Vec n a
reverse = go Nil
  where
    go :: forall m p. Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil                     = case plusZero (length acc) of Refl -> acc
    go acc (x :> (xs :: Vec p' a))
      = case plusSucc @p' (length acc) of Refl -> go (x :> acc) xs

-- VecList
data VecList :: [Nat] -> Type -> Type where
  VLNil :: VecList '[] a
  (:>>) :: Vec n a -> VecList ns a -> VecList (n : ns) a
infixr 5 :>>

deriving instance Eq a   => Eq (VecList ns a)
deriving instance Show a => Show (VecList ns a)

concat :: VecList ns a -> Vec (Sum ns) a
concat VLNil      = Nil
concat (v :>> vs) = v ++ concat vs

-- EVec
data EVec :: (Nat -> Type) -> Type -> Type where
  EVec :: pf n -> Vec n a -> EVec pf a

instance Show a => Show (EVec p a) where
  show (EVec _ v) = show v

-- AlwaysTrue
data AlwaysTrue :: Nat -> Type where
  Always :: AlwaysTrue n

concatMap :: (a -> EVec p b) -> Vec n a -> EVec AlwaysTrue b
concatMap _ Nil       = EVec Always Nil
concatMap f (x :> xs) = case (f x, concatMap f xs) of
  (EVec _ v1, EVec _ v2) -> EVec Always (v1 ++ v2)

-- Greater-than-or-equals
data (:>=:) :: Nat -> Nat -> Type where
  GTEZero :: n :>=: Zero
  GTESucc :: n :>=: m -> Succ n :>=: Succ m

gteSuccLeft :: n :>=: m -> Succ n :>=: m
gteSuccLeft GTEZero      = GTEZero
gteSuccLeft (GTESucc pf) = GTESucc (gteSuccLeft pf)

gteRefl :: SNat n -> n :>=: n
gteRefl SZero      = GTEZero
gteRefl (SSucc n') = GTESucc (gteRefl n')
