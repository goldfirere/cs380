-- NatVec.hs
-- Defines type-level Nats, Vecs, and the other usual goodies

{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module NatVec where

import Data.Kind ( Type )
import Control.Applicative ( Alternative, empty )

-- Nats
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- Fins
data Fin :: Nat -> Type where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)
deriving instance Eq (Fin n)

-- an Alternative is a type that allows failure
-- essentially a generalization of Maybe
toFin :: Alternative f => SNat n -> Integer -> f (Fin n)
toFin SZero     _ = empty
toFin (SSucc _) 0 = pure FZero
toFin (SSucc n) x = FSucc <$> toFin n (x-1)

fromFin :: Fin n -> Integer
fromFin FZero = 0
fromFin (FSucc n) = 1 + fromFin n

instance Show (Fin n) where
  show = show . fromFin

-- Vecs
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Eq a   => Eq   (Vec n a)
deriving instance Show a => Show (Vec n a)

-- indexing a Vec by a Fin
(!!>) :: Vec n a -> Fin n -> a
v !!> f = case (f, v) of
  (FZero,    x :> _)  -> x
  (FSucc f', _ :> xs) -> xs !!> f'
