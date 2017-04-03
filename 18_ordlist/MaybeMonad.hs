{-# LANGUAGE GADTs #-}

module MaybeMonad where

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Show

toNat :: Int -> Maybe Nat
toNat n | n < 0 = Nothing
toNat 0 = Just Zero
toNat n = do
  n' <- toNat (n-1)
  Just (Succ n')
{-
toNat n = case toNat (n-1) of
  Nothing -> Nothing
  Just n' -> Just (Succ n')
-}

toNats :: [Int] -> Maybe [Nat]
toNats [] = Just []
toNats (x:xs) = do
  x' <- toNat x
  xs' <- toNats xs
  Just (x' : xs')
