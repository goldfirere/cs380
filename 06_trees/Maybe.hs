-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction #-}

module Maybe where

import Prelude hiding ( lookup )

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((a,b):xs)
  | x == a    = Just b
  | otherwise = lookup x xs

maybeAdd :: Int -> Maybe Int -> Int
maybeAdd x Nothing  = x
maybeAdd x (Just y) = x + y

addList :: String -> Int -> [(String, Int)] -> Int
addList target x list = case lookup target list of
  Nothing -> x
  Just y  -> x + y

x :: Int
x = fromInteger 3

fib :: (Eq a, Num a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- y :: Double
y = fib 100

z = y + y

plus a = (+) a
