{-# LANGUAGE GADTs, TypeInType, TypeOperators #-}

module ClassOrdList where

import Data.Kind ( Type )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data n :<=: m where
  LTEZero :: Zero :<=: m
  LTESucc :: n :<=: m -> Succ n :<=: Succ m

-- Nat argument is the lowest element in the list
data OrdList :: Nat -> Type where
  One  :: SNat n -> OrdList n
  Cons :: n :<=: m -> SNat n -> OrdList m -> OrdList n

data OrderedList where
  Nol :: OrderedList
  Yol :: OrdList n -> OrderedList
