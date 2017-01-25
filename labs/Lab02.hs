-- Lab02.hs
-- Names:
--
--
-- Read the problems below.
-- When you are done, upload your work in the lab02 directory at our
-- classwork repo, at https://github.com/bmc-cs380/classwork
-- Please rename the file before upload so you don't clobber your
-- classmates' work!
--

{-# OPTIONS -Wall -Wno-type-defaults #-}

module Lab02 where

-- The "Prelude" is the module automatically imported into all Haskell
-- programs. But we'll be redefining some Prelude functions in this file,
-- so we don't want GHCi to get confused by having both your and the
-- Prelude's in scope. So we hide some imports.
-- (By the way, the Prelude is documented here:
--  http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html)
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)

import Test.HUnit

--------------------------------------------------------------------------------
-- The code in this section works -- the test passes. (Run runTestTT testStyle
-- to see this.) But the style is awful. Fix.

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , treverse ]


abc x y z =
  if x then if y then True else
       if (x && z) then True else False
  else False


tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]


reverse l  = reverse_aux l [] where
  reverse_aux l acc =
    if null l then acc
       else reverse_aux (tail l) (head l : acc)


treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

--------------------------------------------------------------------------------
-- Below are many functions over lists. Write the functions and tests over those
-- functions.

testLists :: Test
testLists = "testLists" ~: TestList [ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: assertFailure "testcase for takeWhile"


-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3

tfind :: Test
tfind = "find" ~: assertFailure "testcase for find"


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

tall :: Test
tall = "all" ~: assertFailure "testcase for all"


-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.

tmap2 :: Test
tmap2 = "map2" ~: assertFailure "testcase for map2"


-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]

tzip :: Test
tzip = "zip" ~: assertFailure "testcase(s) for zip"


-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]

ttranspose :: Test
ttranspose = "transpose" ~: assertFailure "testcase for transpose"


-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]

tconcat :: Test
tconcat = "concat" ~: assertFailure "testcase for concat"


-- concatMap

-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]

tconcatMap :: Test
tconcatMap = "concatMap" ~: assertFailure "testcase for concatMap"


    assert $ pass @=? cases s0 - (errors s0 + failures s0)
