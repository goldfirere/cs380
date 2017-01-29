{- Lists.hs

   Demonstrates lists in a variety of ways, including list comprehensions.
-}

module Lists where

-- This is a plain old list of numbers
boring :: [Integer]
boring = [1,2,8,2,5,0]

-- The empty list is written with [] and is pronounced "nil"
nil :: [a]
nil = []

-- The (:) operator (pronounced "cons") adds an element to the beginning
-- of a list. The type of (:) is a -> [a] -> [a]
abc :: [Char]
abc = 'a' : 'b' : 'c' : []
  -- This is the same as ['a', 'b', 'c'] or even "abc"

-- Naturally, (:) can be curried. Here, we map ('x' :) onto a list of strings
-- to prepend each string with an x.
xstrings :: [String]
xstrings = map ('x' :) ["hi", "there", "list", "of", "strings"]

-- You can even have lists of lists
nested :: [[Int]]
nested = [ [1,2,3], [4,5,6], [9998] ]

-- Isn't functional programming fun?  This uses sum :: Num a => [a] -> a
sumOfNested :: [[Int]] -> Int
sumOfNested = sum . map sum

-- RANGES

-- Haskell provides ranges
oneDigitNumbers :: [Int]
oneDigitNumbers = [0..9]

aThroughZ :: [Char]
aThroughZ = ['a' .. 'z']

-- You can even have a range that increments by a user-specified amount
odds :: [Int]
odds = [1,3..19]

-- INFINITE LISTS

-- Haskell is lazy, which means that it can work with infinitely-sized lists.
-- In practice, the list is created only on demand. So as long as you never need
-- the whole list, you won't generate it.
zeroes :: [Integer]
zeroes = 0 : zeroes
  -- Don't print this out! Instead, say something like `take 5 zeroes`.
  -- This uses take :: Int -> [a] -> [a], which takes a prefix of a list and
  -- returns it.

naturals :: [Integer]
naturals = 0 : map (+1) naturals

-- The list of all the Fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Of course, Haskell lets you leave off the end of a range to make an infinite
-- list
allPosEvens :: [Integer]
allPosEvens = [2,4..]

-- LIST COMPREHENSIONS

-- Haskell provides *list comprehensions*, which work like
-- Set-builder notation: https://en.wikipedia.org/wiki/Set-builder_notation

-- divisors returns a list of all divisors of a number
divisors :: Integer -> [Integer]
divisors n = [ d | d <- [2..n-1], n `mod` d == 0 ]
  -- You can read this as:
  -- divisors n = the list of all d such that d is in the range [2..n-1] and
  -- d divides into n.

-- It's now easy to write a primality test. (Not terribly efficient, though.)
isPrime :: Integer -> Bool
isPrime x = x >= 2 && null (divisors x)
  -- This uses (null :: [a] -> Bool) which checks for an empty list

-- all the prime Fibonacci numbers. Humankind does not know whether this list
-- is infinite: https://en.wikipedia.org/wiki/Fibonacci_prime
primeFibs :: [Integer]
primeFibs = [ f | f <- fibs, isPrime f ]

-- List comprehensions can compute a Cartesian product:
prod :: [a] -> [b] -> [(a,b)]
prod xs ys = [ (x,y) | x <- xs, y <- ys ]

-- This is a list of pairs of numbers whose product has a last digit of "3"
endsIn3 :: [(Integer, Integer)]
endsIn3 = [ (a,b) | a <- [1..15], b <- [1..15], a*b `mod` 10 == 3 ]

-- Finds words whose reverse is also a word. (Very inefficient!)
flippers :: [String] -> [String]
flippers words = [ word | word <- words, reverse word `elem` words ]
