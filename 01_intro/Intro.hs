module Intro where

f x = x + 3

g x y = x^2 - y^2

h :: Bool -> Char
h b = if b then 'a' else 'c'

-- fib n = if n <= 1 then n else fib (n-1) + fib (n-2)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

ignoreFirst :: a -> b -> b
ignoreFirst _ y = y

-- sumDigits n = if n == 0 then 0 else (mod n 10) + (sumDigits (div n 10))
{- longer
comment -}

sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)
