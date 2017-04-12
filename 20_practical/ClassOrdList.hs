{-# LANGUAGE GADTs, TypeInType, TypeOperators, StandaloneDeriving,
             ScopedTypeVariables, TypeFamilies, AllowAmbiguousTypes #-}

module ClassOrdList where

import Data.Kind ( Type )
import Data.Type.Equality

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
{-
data ExSNat where
  ExSNat :: SNat n -> ExSNat
-}

type ExSNat = Ex SNat

data ExOrdList where
  ExOrdList :: OrdList n -> ExOrdList

-- ExOrdList = Ex OrdList

data Ex :: forall k. (k -> Type) -> Type where
  Ex :: forall (t :: k -> Type) (x :: k). t x -> Ex t

fromOrderedList :: OrderedList -> [Int]
fromOrderedList Nol     = []
fromOrderedList (Yol l) = fromOrdList l

fromOrdList :: OrdList n -> [Int]
fromOrdList (One x) = [fromNat (fromSNat x)]
fromOrdList (Cons _ x xs) = fromNat (fromSNat x) : fromOrdList xs

fromSNat :: SNat n -> Nat
fromSNat SZero = Zero
fromSNat (SSucc n) = Succ (fromSNat n)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

instance Show OrderedList where
  show = show . fromOrderedList

list = Yol (Cons (LTESucc LTEZero) (SSucc SZero) (One (SSucc (SSucc SZero))))

toOrderedList :: [Int] -> Maybe OrderedList
toOrderedList [] = Just Nol
toOrderedList (x : xs) = do
  Ex ord_list <- toOrdList x xs
  Just (Yol ord_list)

toOrdList :: Int -> [Int] -> Maybe (Ex OrdList)
toOrdList x [] = do
  n <- toNat x
  case toSNat n of Ex n' -> Just (Ex (One n'))
toOrdList x (y:ys) = do
  Ex ys' <- toOrdList y ys
  n <- toNat x
  case toSNat n of Ex n' -> case n' <=>? headOL ys' of
                              Left lte -> Just (Ex (Cons lte n' ys'))
                              Right _  -> Nothing

headOL :: OrdList n -> SNat n
headOL (One n)      = n
headOL (Cons _ h _) = h

(<=>?) :: SNat n -> SNat m -> Either (n :<=: m) (m :<=: n)
SZero     <=>? _     = Left LTEZero
(SSucc _) <=>? SZero = Right LTEZero
(SSucc n') <=>? (SSucc m') =
  case n' <=>? m' of
    Left n'_lte_m'  -> Left (LTESucc n'_lte_m')
    Right m'_lte_n' -> Right (LTESucc m'_lte_n')

toNat :: Int -> Maybe Nat
toNat n | n < 0 = Nothing
toNat 0 = Just Zero
toNat n = do
  n' <- toNat (n-1)
  Just (Succ n')

toSNat :: Nat -> Ex SNat
toSNat Zero = Ex SZero
toSNat (Succ n) = case toSNat n of Ex n' -> Ex (SSucc n')

ordList = toOrderedList [1,2,3]
bad     = toOrderedList [3,2,1]

type family Min n m where
  Min Zero     _        = Zero
  Min _        Zero     = Zero
  Min (Succ n) (Succ m) = Succ (Min n m)

lte_min :: n :<=: m -> (Min n m :~: n)
lte_min LTEZero = Refl
lte_min (LTESucc lte) = case lte_min lte of Refl -> Refl

min_comm :: SNat n -> SNat m -> Min n m :~: Min m n
min_comm SZero _ = Refl
min_comm _ SZero = Refl
min_comm (SSucc n') (SSucc m') = case min_comm n' m' of Refl -> Refl

insert :: Nat -> OrderedList -> OrderedList
insert n ol = case toSNat n of
  Ex sn -> case ol of
    Nol          -> Yol (One sn)
    Yol ord_list -> Yol (insert_ord_list sn ord_list)
  where
    insert_ord_list :: SNat n -> OrdList m -> OrdList (Min n m)
    insert_ord_list sn (One sm) =
      case sn <=>? sm of
        Left sn_lte_sm -> case lte_min sn_lte_sm of
                            Refl -> Cons sn_lte_sm sn (One sm)
        Right sm_lte_sn ->
          case lte_min sm_lte_sn of
            Refl -> case min_comm sn sm of
              Refl -> Cons sm_lte_sn sm (One sn)

    insert_ord_list sn tail@(Cons lte sm rest) =
      case sn <=>? sm of
        Left sn_lte_sm -> case lte_min sn_lte_sm of
                            Refl -> Cons sn_lte_sm sn tail
        Right sm_lte_sn ->
          case lte_min sm_lte_sn of
            Refl -> case min_comm sn sm of
              Refl -> Cons _ sm (insert_ord_list sn rest)
