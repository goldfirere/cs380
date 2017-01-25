-- Lab01.hs
-- CS 380
-- Spring 2017

module Lab01 where

-- This file should compile as-is in GHCi. Test this by loading it into GHCi:
-- navigate to the directory containing this file and run
--   > ghci Lab01.hs
-- The file should load in GHCi.

doNothing :: Int -> Int
doNothing x = x

-- With the above definition, you should be able to say
--   Lab01> :t doNothing
-- and get back that the type of doNothing is Int -> Int.
-- Type in
--   Lab01> doNothing 5
-- and see what happens.

-- Experiment a bit with GHCi, entering in expressions such as
-- 3 + 5 or "Hi" + True to see what happens.

-- Now we're ready to start in earnest:

{-
1. Write a function named |add1| that takes an |Int| and returns an |Int| that
is one greater than its input. For example, if we compute |add1 5|, we should
get |6|.
-}

add1 :: Int -> Int
add1 x = undefined

{-
2. Write a function named |always0| that takes an |Int| and returns an |Int|.
The return value should always just be 0.
-}

-- add a type signature for always0 here:
always0 n = undefined

{-
3. Write a function |subtract| that takes two numbers (that is, |Int|s) and subtracts them.
(Even though this and future exercises do not ask you to write a type signature,
all top-level definitions in a file should have type signatures. Start this good
habit now.)
-}

{-
4. Write a function |addmult| that takes three numbers. Let's call them |p|, |q|, and |r|.
|addmult| should add |p| and |q| together and then multiply the result by |r|.
-}

-- The next function will also need you to use an |if| expression in Haskell. Here is an
-- example of |if| in action:

greaterThan0 :: Int -> String
greaterThan0 n = if n > 0 then "Yes!" else "No :("

-- Note that both the |then| part \emph{and} the |else| part are required in
-- Haskell. If you left the |else| out, what would happen if |n| weren't greater than 0?
-- There's no good answer to that question, so the |else| is always required.

{-
5. Write a function |myAbs| that computes absolute value. (Don't use the
built-in function |abs| --- that's cheating!)
-}

{-
6. Write a function |pushOut| that takes a number and returns the number
that is one step further from 0. That is, |pushOut 3| is |4|, |pushOut (-10)|
is |(-11)|, and |pushOut 0| is |0|. That last one is because we don't know
which direction to go! Note that, in Haskell, you *always* have to
put parentheses around negative numbers.

Hint: Use == for equality checking in Haskell, just like Java.
-}

-- All of the functions so far have dealt only with numbers. Now, we'll look
-- at |String|s, which are chunks of printable text. |String|s are written
-- in double-quotes in Haskell:

exampleString :: String
exampleString = "Hello there!"

-- (You might notice that the |greaterThan0| example uses |String|s.) There are
-- two interesting operations on |String|s (for now):
--   * Use the ++ operator to concatenate |String|s. To concatenate is to put
-- one after the other. For example, |"Hi " ++ "there!"| is |"Hi there!"|. This
-- is quite like + on |String|s in Java.
--
--   * Use |show| to convert most types into |String|s. For example, |show 3|
--   is |"3"|. This is quite like |toString| in Java, but Java also automatically
--   converts things to |String|s when you use + with another |String|.

{-
7. Write a function |greet| (with type |String -> String|) that takes in a
person's name and says |"Hi "| to that person. For example,
|greet "Haskell"| is |"Hi Haskell"|. (The language Haskell is named after
a logician, Haskell Curry.)
-}

{-
8. Write a function |greet2| that is just like |greet|, but if the name
provided is empty, your function should return |"Hi there"|. So, giving
an empty string, written |""|, is like giving the string |"there"|.
To test a string for emptiness, use the |null| function, of
type |String -> Bool|. |null ""| is |True|, while |null "Esmerelda"| is |False|.
-}

-- The functions up until now have all been fairly simple. The next function, however,
-- must use recursion.

-- For example, here is a function that makes a |String| containing any number
-- of \texttt{a}s:

makeAs :: Int -> String
makeAs n =  if n == 0
            then  ""
            else  "a" ++ makeAs (n-1)

-- For example, |makeAs 3| is |"aaa"| and |makeAs 7| is |"aaaaaaa"|.

{-
9. Write a function |twiceAs| that is like |makeAs|, but it makes twice
as many 'a's as requested.
-}

{-
10. Write a function |countDown| (with type |Int -> String|) that produces a
|String| counting down from a number. For example |countDown 5| is
|"5 4 3 2 1 "|. Note that there is an extra space at the end --- that's
supposed to make it easier. (Bonus points if you can get rid of the
extra space!) If the number passed in is |0| or less, the returned
|String| should be |"Too low"|. Remember that |show| converts a number
to a |String|.
-}

{-
11. Write a function |countUp| that goes the opposite way of |countDown|.
-}

{-
12. Write a function |mult| (with type |Int -> Int -> Int|) to multiply two
numbers without using built-in multiplication. To do this, you will use
repeated addition.
To compute |mult a b|, check |b|. If |b| is |0|, then |mult a b| should
be |0|. If |b| is greater than 0, |mult a b| should be |a| plus the
result of |mult a (b-1)|.
-}

{-
13. Write a function |power| that raises a number |a| to the power |b|.
This is quite similar to the last exercise.
-}

{-
14. Write a function |isEven| that returns |True| whenever the passed-in
number is even and |False| otherwise. Note: the type of |True| and |False|
is |Bool|, and this does not have to be recursive.
-}

{-
15. Write a function |sumDigits| that computes the sum of the digits
in a number. The Haskell function |mod| computes the modulus of one
number with respect to another; |mod n 10| therefore is |n|'s last
digit. The Haskell function |div| divides two numbers using integer
division; |div n 10| therefore is the number |n| missing its last digit.
-}

{-
16. Write a function |reverseDigits| that reverses the order of digits
in a number. Accordingly |reverseDigits 12345| is |54321|.
-}

-- Sometimes, a function isn't quite defined correctly to be recursive.
-- That is, a particular operation cannot be expressed in terms of the
-- same operation on smaller arguments. A good example of this is
-- primality checking: if you want to know whether 5 is prime, it does
-- not help much to know that 4 is not prime. In this case, you use
-- a *helper function*, a separate function that does the internal work
-- necessary. The following exercises require helper functions.

{-
17a. Write a function |hailstoneStep| to compute the subsequent number
in the hailstone sequence. See https://en.wikipedia.org/wiki/Collatz_conjecture
Examples: |hailstoneStep 5| = 16; |hailstoneStep 8| = 4.
-}

{-
17b. Write a function |collatz| that evaluates the length of the hailstone
sequence starting at the given number.
Examples: |collatz 1| = 1; |collatz 4| = 3; |collatz 12| = 10.
-}

-- The Collatz Conjecture asks whether |collatz| terminates for all possible
-- inputs. The answer to this question is unknown to humankind as of Jan. 2017.

{-
18. Write a function |isPrime| that checks whether or not a number is prime.
An easy (but not terribly efficient) way to check for primality of |n| is to check
all numbers |m| such that |2 <= m < n| to see if |m| evenly divides |n|. If no
such |m| divides into |n|, then |n| is prime.
-}
