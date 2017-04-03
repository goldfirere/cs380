{-# LANGUAGE TypeInType, GADTs, TypeOperators, StandaloneDeriving #-}

module OrdList where

import Data.Kind ( Type )
import Prelude hiding ( head )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data (:>=:) :: Nat -> Nat -> Type where
  GTEZero :: n :>=: Zero
  GTESucc :: n :>=: m -> Succ n :>=: Succ m

data OrdList :: Nat -> Type where
  One :: SNat n -> OrdList n
  Cons :: n1 :>=: n2 -> SNat n2 -> OrdList n1 -> OrdList n2

ordListToList :: OrderedList -> [Int]
ordListToList Nol = []
ordListToList (Yol ol) = go ol
  where
    go :: OrdList n -> [Int]
    go (One x) = [fromNat (fromSNat x)]
    go (Cons _ x xs) = fromNat (fromSNat x) : go xs

data Ex :: (k -> Type) -> Type where
  Ex :: t x -> Ex t

data OrderedList where
  Nol :: OrderedList
  Yol :: OrdList n -> OrderedList

instance Show OrderedList where
  show = show . ordListToList

toSNat :: Nat -> Ex SNat
toSNat Zero = Ex SZero
toSNat (Succ n) = case toSNat n of Ex sn -> Ex (SSucc sn)

fromSNat :: SNat n -> Nat
fromSNat SZero = Zero
fromSNat (SSucc n) = Succ (fromSNat n)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

toNat :: Int -> Maybe Nat
toNat n | n < 0 = Nothing
toNat 0 = Just Zero
toNat n = do
  n' <- toNat (n-1)
  return (Succ n')

(>=?) :: SNat n1 -> SNat n2 -> Maybe (n1 :>=: n2)
_ >=? SZero = Just GTEZero
SSucc n1 >=? SSucc n2 = fmap GTESucc (n1 >=? n2)
_ >=? _ = Nothing

head :: OrdList n -> SNat n
head (One n) = n
head (Cons _ n _) = n

checkOL :: [Int] -> Maybe OrderedList
checkOL [] = Just Nol
checkOL (x:xs) = do
  Ex ol <- go x xs
  return (Yol ol)

  where
    go :: Int -> [Int] -> Maybe (Ex OrdList)
    go x [] = do
      n <- toNat x
      case toSNat n of Ex sn -> Just (Ex (One sn))

    go x (y:ys) = do
      Ex ys' <- go y ys
      Ex sn <- fmap toSNat (toNat x)
      gte <- head ys' >=? sn
      Just (Ex (Cons gte sn ys'))
