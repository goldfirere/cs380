-- FoldMap.hs
--
-- This file contains some FoldMap examples that might be good inspiration for hw04.

module FoldMap where

import Data.Monoid

-- foldMap :: Monoid m => (a -> m) -> [a] -> m, although the library version is actually
-- a bit more general than that

-- using foldMap to sum up a list:
sumList :: Num a => [a] -> a
sumList = getSum . foldMap Sum

-- using foldMap to product up a list:
prodList :: Num a => [a] -> a
prodList = getProduct . foldMap Product

-- using foldMap to get the first even number in a list:
-- this has to return a Maybe, because the list might be empty or have no evens
firstEven :: Integral a => [a] -> Maybe a
firstEven = getFirst . foldMap (First . check_even)
  where
    check_even n
      | even n    = Just n
      | otherwise = Nothing
  -- Note that this is not the best way of finding the first even number
  -- in a list! The "find" function is better.
