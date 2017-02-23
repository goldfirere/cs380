{-# LANGUAGE GADTSyntax, NoMonomorphismRestriction #-}

module Class where

import Prelude hiding ( Monoid(..), Functor(..) )
import Hw02
import Peano

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

(<>) = mappend

instance Monoid [a] where
  mempty = []
  mappend = (++)

data Sum a where
  Sum :: { getSum :: a } -> Sum a

-- data Sum a = Sum { getSum :: a }

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a + b)

data Product a where
  Product :: { getProduct :: a } -> Product a

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product a) (Product b) = Product (a * b)

x = getSum (Sum 3 <> Sum 4)
y = getProduct (Product 3 <> Product 4)

{-
instance Monoid Integer where
  mempty = 0
  mappend = (+)

instance Monoid Integer where
  mempty = 1
  mappend = (*)
-}
-- x = 3 <> 4

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just x) = Just (f x)
  fmap _ Nothing  = Nothing

instance Functor Tree where
  fmap = mapTree

instance Functor [] where
  fmap = map

len :: [] a -> Int
len [] = 0
len (_:xs) = 1 + len xs
{-
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b -- pronounced "bind"
-}
{-
instance Monad Maybe where
  return x = Just x

  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Just x  >>= f = f x
  Nothing >>= _ = Nothing
-}
divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide a b = Just (a `div` b)

divideList :: Int -> [Int] -> Maybe Int
divideList numerator [] = Just numerator
divideList numerator (x:xs) = do
  result <- divide numerator x
  divideList result xs
{-
divideList numerator (x:xs)
  = divide numerator x >>= \result -> divideList result xs
-}
{-
divideList numerator (x:xs) = case divide numerator x of
  Just result -> divideList result xs
  Nothing     -> Nothing
-}
