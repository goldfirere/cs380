-- Testing.hs
--
-- Demonstrates the use of HUnit to write unit tests
--
-- If your system complains that Test.HUnit is not found,
-- run this from your command line:
--
--    cabal update
--    cabal install HUnit
--
-- `cabal` is the name of one of Haskell's package managers. It installs
-- packages accessible globally on your computer, by any Haskell program.
-- `stack` is the other main package manager, which prefers to install
-- packages locally, accessible only to one project. By doing so, `stack`
-- avoids conflicts that can arise between certain packages. I recommend
-- using `cabal` in this course, as we will not need a tremendous number
-- of outside packages. However, if you wish to use `stack`, by all means
-- do so.
--
-- NB: You can find packages to download at hackage.haskell.org. The
-- documentation for packages is also there. For example, the documentation
-- for Test.HUnit is at
-- http://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit.html
-- (click on the link to Test.HUnit.Base at the bottom to see the individual
-- functions).
--

module Testing where

import Test.HUnit

-- gets the length of a list
len :: [a] -> Int
len [_]      = 1
len (_ : xs) = 1 + len xs
len [] = 0

len_test :: Test
len_test = TestList [ "abc"   ~: len "abc"   ~?= 3
                    , "hello" ~: len "hello" ~?= 5
                    , ""      ~: len ""      ~?= 0 ]
  -- This demonstrates several aspects of Haskell and HUnit:
  --   1. Strings are lists. This is convenient.
  --   2. (~:) :: String -> Test -> Test
  --      labels a test during printing
  --   3. (~?=) :: Eq a => a -> a -> Test
  --      checks to see if the actual value (on the left) equals an
  --      expected value (on the right). If the actual value is on the
  --      right, use (~=?). Mnemonic: put the ? toward the unknown
  --      quantity.
  --
  --   Actually, the types of (~:) and (~?=) are a little more involved,
  --   but the intuition above works.

  -- to run a Test, use, e.g.,
  -- runTestTT len_test

prime :: Int -> Bool
prime n
  | n < 2 = False
  | otherwise
  = null (filter ((== 0) . (n `mod`)) test_divisors)
  where {
     -- a list of integers from 2 up to the square root of n
    test_divisors = range 2 (round (sqrt (fromIntegral n)));

     -- generate a list going from lo (inclusive) to hi (exclusive)
         range lo hi
      | lo >= hi  = []
      | otherwise = lo : range (lo+1) hi; }

prime_test :: Test
prime_test = TestList [ "1" ~: prime 1 ~?= False
                      , "2" ~: prime 2 ~?= True
                      , "3" ~: prime 3 ~?= True
                      , "4" ~: prime 4 ~?= False ]

-- an easier way to write prime_test
prime_test' :: Test
prime_test' = TestList (zipWith (\n is_prime ->
                                 show n ~: prime n ~?= is_prime)
                                [1,     2,    3,    4]
                                [False, True, True, False])
