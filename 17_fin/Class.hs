{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications #-}

module Class where

import Data.Kind ( Type )
import Prelude hiding ( (++), concat, length )
import qualified Prelude
import Data.Type.Equality

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)

data Vec :: Nat -> Type -> Type where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil ++ v2 = v2
(x :> xs) ++ v2 = x :> (xs ++ v2)
{-
append :: Vec n a -> Vec m a -> Vec (m + n) a
append v1 v2 = v1 ++ v2
-}
listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (xs : xss) = xs Prelude.++ listConcat xss

type family a * b where
  Zero   * b = Zero
  Succ a * b = b + (a * b)

concatVec :: Vec m (Vec n a) -> Vec (m * n) a
concatVec Nil = Nil
concatVec (xs :> xss) = xs ++ concatVec xss

stuffs = (1 :> 2 :> 3 :> Nil)
       :>> (4 :> 5 :> Nil)
       :>> (6 :> Nil)
       :>> Nil
       :>> (7 :> Nil)
       :>> VLNil

data VecList :: [Nat] -> Type -> Type where
  VLNil :: VecList '[] a
  (:>>) :: Vec n a -> VecList ns a
        -> VecList (n : ns) a
infixr 5 :>>

type family Sum ns where
  Sum '[]      = Zero
  Sum (n : ns) = Sum ns + n

concat :: VecList ns a -> Vec (Sum ns) a
concat VLNil = Nil
concat (xs :>> xss)
  = case plus_comm (length xs) (length (concat xss)) of Refl -> xs ++ concat xss

length :: Vec n a -> SNat n
length Nil = SZero
length (_ :> xs) = SSucc (length xs)

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

plus_zero :: SNat n -> n + Zero :~: n
plus_zero SZero      = Refl
plus_zero (SSucc n') = {- case plus_zero n' of Refl -> -} Refl

plus_succ :: forall m n. SNat n -> (n + Succ m) :~: Succ (n + m)
plus_succ SZero      = Refl
plus_succ (SSucc sn') = case plus_succ @m sn' of Refl -> Refl

plus_comm :: SNat n -> SNat m -> n + m :~: m + n
plus_comm SZero                    sm = case plus_zero sm of Refl -> Refl
plus_comm (SSucc (sn' :: SNat n')) sm
--  = case plus_succ @n' sm of Refl -> case plus_comm sn' sm of Refl -> Refl
  = case plus_comm sn' sm of Refl -> case plus_succ @n' sm of Refl -> Refl
