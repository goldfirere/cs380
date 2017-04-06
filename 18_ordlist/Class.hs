{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications, StandaloneDeriving #-}

module Main where

import Data.Kind ( Type )
import Prelude hiding ( (++), concat, length, (!!), reverse, filter )
import qualified Prelude
import Data.Type.Equality
import Text.Read ( readMaybe )

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

deriving instance Show a => Show (Vec n a)

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
  Sum (n : ns) = n + Sum ns

concat :: VecList ns a -> Vec (Sum ns) a
concat VLNil = Nil
concat (xs :>> xss) = xs ++ concat xss

length :: Vec n a -> SNat n
length Nil = SZero
length (_ :> xs) = SSucc (length xs)

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

plus_zero :: SNat n -> n + Zero :~: n
plus_zero SZero      = Refl
plus_zero (SSucc n') = case plus_zero n' of Refl -> Refl

plus_succ :: forall m n. SNat n -> (n + Succ m) :~: Succ (n + m)
plus_succ SZero      = Refl
plus_succ (SSucc sn') = case plus_succ @m sn' of Refl -> Refl

plus_comm :: SNat n -> SNat m -> n + m :~: m + n
plus_comm SZero                    sm = case plus_zero sm of Refl -> Refl
plus_comm (SSucc (sn' :: SNat n')) sm
--  = case plus_succ @n' sm of Refl -> case plus_comm sn' sm of Refl -> Refl
  = case plus_comm sn' sm of Refl -> case plus_succ @n' sm of Refl -> Refl

type family Replicate (n :: Nat) (x :: a) :: [a] where
  Replicate Zero     _ = '[]
  Replicate (Succ n) x = x ': Replicate n x

explode :: Vec n a -> VecList (Replicate n (Succ Zero)) a
explode Nil       = VLNil
explode (x :> xs) = (x :> Nil) :>> explode xs

data Fin :: Nat -> Type where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)

(!!) :: Vec n a -> Fin n -> a
vec !! fin = case (fin, vec) of
  (FZero,   x :> _)  -> x
  (FSucc n, _ :> xs) -> xs !! n

{-
terrible :: Fin n -> Vec n a -> a
terrible FZero     (x :> _)  = x
terrible (FSucc n) (_ :> xs) = terrible n xs
-}

{-
(!!!) :: Vec (Succ n) a -> Fin (Succ n) -> a
(x :> _) !!! FZero = x
(_ :> xs) !!! (FSucc n) = xs !!! n

no_fin_zero ::
-}

-- toFin :: Int -> Fin n

data AlwaysTrue :: Nat -> Type where
  Always :: AlwaysTrue n

toVec :: [a] -> EVec AlwaysTrue a   --  toVec :: [a] -> Vec n a
                         --  toVec :: [a] -> (exists n. Vec n a)
toVec [] = EVec Always Nil
toVec (x:xs) = case toVec xs of
  EVec _ ys -> EVec Always (x :> ys)

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil x = x :> Nil
snoc (y :> ys) x = y :> snoc ys x

reverse :: Vec n a -> Vec n a
reverse Nil = Nil
reverse (x :> xs) = snoc (reverse xs) x


main = do
  nums <- readInNumbers
  case toVec nums of EVec _ xs -> print (reverse xs)

readInNumbers :: IO [Integer]
readInNumbers = do
  line <- getLine
  case readMaybe line of
    Nothing -> return []
    Just n  -> do
      more_nums <- readInNumbers
      return (n : more_nums)

data EVec :: (Nat -> Type) -> Type -> Type where
  EVec :: proof m -> Vec m a -> EVec proof a

data (:>=:) :: Nat -> Nat -> Type where
  GTEZero :: n :>=: Zero
  GTESucc :: (n :>=: m) -> (Succ n :>=: Succ m)

gteSuccLeft :: (n :>=: m) -> (Succ n :>=: m)
gteSuccLeft GTEZero       = GTEZero
gteSuccLeft (GTESucc gte) = GTESucc (gteSuccLeft gte)

 -- ((:>=:) n) really should be (n :>=:)
filter :: (a -> Bool) -> Vec n a -> EVec ((:>=:) n) a
filter _ Nil       = EVec GTEZero Nil
filter f (x :> xs) = case filter f xs of
  EVec gte fxs -> if f x then EVec (GTESucc gte) (x :> fxs)
                  else EVec (gteSuccLeft gte) fxs

-- badfilter :: (a -> Bool) -> Vec n a -> Vec m a
-- badfilter _ Nil       = Nil
-- badfilter f (x :> xs) = case filter f xs of EVec fxs -> if f x then EVec (x :> fxs)
--                                                            else EVec fxs

gteRefl :: SNat n -> n :>=: n
gteRefl SZero     = GTEZero
gteRefl (SSucc m) = GTESucc (gteRefl m)

gteRefl' :: Vec n a -> n :>=: n
gteRefl' Nil = GTEZero
gteRefl' (_ :> xs) = GTESucc (gteRefl' xs)

tail :: Vec n a -> EVec ((:>=:) n) a
tail Nil = EVec GTEZero Nil
-- tail (_ :> xs) = EVec (gteSuccLeft (gteRefl (length xs))) xs
tail (_ :> xs) = EVec (gteSuccLeft (gteRefl' xs)) xs
