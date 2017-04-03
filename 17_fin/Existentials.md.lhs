---
title: Existentials lecture notes
---

Existential data types
======================

All of this working with `Vec` is fun and all, but it surely seems impractical.
After all, if the length of a list is known at compile time, why not just use an
array? (Yes, Haskell has [two](http://hackage.haskell.org/package/array)
[different](http://hackage.haskell.org/package/vector) implementations of arrays,
both with a packed memory representation.) Indeed, this is a good question. The
convenience of linked lists is that we don't need to know their lengths up front.

It turns out that we *don't* need to know a `Vec`'s length at compile time. The
secret is to use existentials.

Let's get our Literate Haskell header out of the way:

> {-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeOperators #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}

> module Existentials where

> import Data.Kind ( Type )
> import Prelude hiding ( reverse, filter )
> import Text.Read ( readMaybe )

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

> deriving instance Show a => Show (Vec n a)

Hiding a `Vec`'s length
-----------------------

Suppose we want to write a program that asks the user for a bunch of numbers,
continuing to read in numbers until a non-number is entered. We will put these
numbers in a list, reverse the list, and print out the reversed list. This is
perhaps not the most inspiring of programs, but it serves to show how existentials
are useful. (Indeed, without existentials, little of this fancy type stuff would
be useful!)

Naturally, instead of using `Data.List`'s `reverse` function, we want to use
our `reverse` on `Vec`s. This `Vec` `reverse` is better than `Data.List`'s:
GHC can verify that our `reverse` does not change the length of the list;
`Data.List`'s function offers no such reassurance. To pull this off, we'd
like to write

```haskell
toVec :: [a] -> Vec n a
toVec []       = Nil
toVec (x : xs) = x :> toVec xs
```

The problem is that this function cannot be written. When we write a type variable
in a type signature, that is understood to be universally quantified. That is,
the definition should work for *all* choices of that variable. (We can see this
when we think about a function `bad :: a -> b`. There is no implementation that
works for *all* `a` and `b`, even though `bad x = not x` works for *some* `a`
and *some* `b`.) Indeed, compiling the definition above gives us

    • Couldn't match type ‘n’ with ‘'Succ n0’
      ‘n’ is a rigid type variable bound by
        the type signature for:
          toVec :: forall a (n :: Nat). [a] -> Vec n a

This comes from the second equation, where GHC wants the result of `:>` to be
a `Vec (Succ n0) a` for some `n0`, but all we know is that the result should have
the index `n`. The first equation leads to a similar error, but about `Zero`.

Instead, we need an *existential* type, like this:

> data ExVec :: Type -> Type where
>   ExVec :: Vec n a -> ExVec a

The `ExVec` constructor takes any `Vec n a` and packs it into a `ExVec a`. Note that
the `n` does not appear in the result. That's what makes the `n` existential. When
we have a value of type `ExVec a`, then we know that there exists an `n` such that
the `ExVec` contains a `Vec n a`, but we don't know what that `n` is -- it's not in
the type.

Let's walk through a few examples to understand this better. (This is called
`toVec1` because we're going to refine this later into `toVec`.)

> toVec1 :: [a] -> ExVec a
> toVec1 []     = ExVec Nil
> toVec1 (x:xs) = case toVec1 xs of ExVec xs' -> ExVec (x :> xs')

This function successfully converts a list into a `Vec`. Note that the output
length is *not* mentioned in the return type of `toVec1`. In this way, an existential
type *hides* one or more type variables. In the body of the function, we must be
careful to pack and unpack the `Vec` into its `ExVec` holder.

In contrast to `toVec1`'s successful use of an existential, here's a poor usage:

```haskell
bad (ExVec vec) = vec
```

I've cleverly avoided giving a type to `bad`, but perhaps GHC can infer it. No dice:

    • Couldn't match expected type ‘t1’ with actual type ‘Vec n t’
        because type variable ‘n’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a pattern with constructor:
          ExVec :: forall a (n :: Nat). Vec n a -> ExVec a,

Well, what would the type of `bad` be? It would have to be something like
`ExVec a -> Vec n a`, where the `n` is the `n` found by looking at the `Vec` stored
in the `ExVec`. Of course, this type is inexpressible, leading to the error message
above. (It complains about scoping. When we reveal the `Vec` inside an `ExVec`
package, that `Vec`'s index `n` is in scope -- but only for the length of the
pattern match. The type of the function we're in is clearly not part of that scope.)

The bottom line here: we can pattern-match against existential constructors, but
we must be careful that any type variables that come into scope don't escape. In
`toVec1`, this happens by re-packing the result into another `ExVec`.

A "practical" program with `Vec`
--------------------------------

Now that we have `toVec1`, we can indeed write a real program over `Vec`s.

First, we'll need a function that reads in numbers from the user until a non-number
is entered:

> readNumbers :: IO [Integer]
> readNumbers = do
>   line <- getLine
>   case readMaybe line of
>     Nothing -> return []
>     Just n  -> do
>       more_nums <- readNumbers
>       return (n : more_nums)

We'll need a few more functions on `Vec`:

> snoc :: Vec n a -> a -> Vec (Succ n) a
> snoc Nil       x = x :> Nil
> snoc (y :> ys) x = y :> (snoc ys x)

> -- Yes, this is slow. But it's just so much easier to write!
> reverse :: Vec n a -> Vec n a
> reverse Nil       = Nil
> reverse (x :> xs) = snoc (reverse xs) x

> fromVec :: Vec n a -> [a]
> fromVec Nil       = []
> fromVec (x :> xs) = x : fromVec xs

Now, we can write a `main` that does all the rest of the work.

> main :: IO ()
> main = do
>   putStrLn "Enter some numbers, one per line. To stop, enter anything else."
>   nums <- readNumbers
>   putStrLn ("You gave me " ++ show nums ++ ".")
>   let reversed = case toVec1 nums of
>         ExVec vnums -> fromVec (reverse vnums)
>   putStrLn ("Reversing gives me " ++ show reversed ++ ".")

You can actually compile this module with `ghc Existentials.md.lhs -main-is Existentials`
to produce an executable that you can run. (The `-main-is` tells GHC to look for the module
named `Existentials` instead of its usual `Main`.)

Let's quickly look at the use of existentials there. The result of `toVec1` is scrutinized
to reveal a `Vec` `vnums` of unknown length `n`. Because of the scoping rules around
existentials, we can't have `n` escape in the type of the right-hand side of the
pattern-match. Happily, that right-hand side has type `[Integer]`, with no mention
of the length. Without the `fromVec`, though, we'd be in big trouble. Try removing the
`fromVec` to see the "would escape its scope" error appear.

To recap what we've done here: these notes have all been about adding more types to
functions. But if those functions only worked on data structures whose type indices
are all known at compile time, they would be barely useful. Here, we've taken a list
entered in by the user (surely of an unknowable length) and converted it to a length-indexed
`Vec` by use of an existential data type. That allowed us to use all our length-verified
algorithms on the user-provided list, converting back to a list only at the end.

Existentials are too loose
--------------------------

Existentials are useful for more than just `main`: they can be used to describe the output
of functions whose output length is unknowable. Consider `filter`:

```haskell
filter :: (a -> Bool) -> Vec n a -> Vec ??????????? a
```

Without knowing the contents of the `Vec` and the behavior of the filtering function, it's
impossible to know what the output length should be. So we don't try. We use an existential:

```haskell
filter :: (a -> Bool) -> Vec n a -> ExVec a
```

However, this is a bit dissatisfying, because we know *something* about the output length:
it's surely no greater than the input length. This information might be useful if, say,
we get an index into the output `Vec` (of the [`Fin`](Fin.html) type) and then want to
use this index on the original input `Vec`. It's sure to be valid. So, we modify the
existential type to allow for some extra information about the hidden type index `n`:

> data EVec :: (Nat -> Type) -> Type -> Type where
>   EVec :: p n -> Vec n a -> EVec p a

The idea here is that `EVec` is parameterized by a predicate over `Nat`s (called `p`),
as well as the element type `a`. Evidence for this predicate is then stored in the
`EVec`.

It's surely time for an example. First, we'll define a type of evidence that one number
is greater than or equal to another:

> data (:>=:) :: Nat -> Nat -> Type where
>   GTEZero :: n :>=: Zero
>   GTESucc :: n :>=: m -> Succ n :>=: Succ m

This type is somewhat like the `:~:` type that proves that one type is equal to another.
It says that there are two ways for `n` to be greater-than-or-equal-to (gte) `m`: either
`m` is `Zero`, or both `n` and `m` are the successors of other numbers, `n'` and `m'`, and
`n'` gte `m'`.

Before we can use this type to write `filter`, we'll need to prove a key fact about `:>=:`:

> gteSuccLeft :: (n :>=: m) -> (Succ n :>=: m)
> gteSuccLeft GTEZero       = GTEZero
> gteSuccLeft (GTESucc gte) = GTESucc (gteSuccLeft gte)

This says that, whenever we know `n :>=: m`, we can prove `Succ n :>=: m`. This fact
should not be surprising, but it can be established only by induction from the definition
of `:>=:`; there is no way for GHC to "guess" this fact.

We can now use these definitions to write `filter`:

> filter :: (a -> Bool) -> Vec n a -> EVec ((:>=:) n) a
> filter _ Nil       = EVec GTEZero Nil
> filter f (x :> xs) = case filter f xs of
>   EVec gte xs'
>     | f x       -> EVec (GTESucc gte)     (x :> xs')
>     | otherwise -> EVec (gteSuccLeft gte) xs'

First, examine the type. The first parameter to `EVec` is `((:>=:) n)`. This takes
the prefix form of the type operator `:>=:` and applies it to `n`. Because it's in
prefix notation, the `n` is the first argument to `(:>=:)`. At the term level,
we might write the section `(n :>=:)`, but sections don't work in types.

Now, let's look at the term-level definition. In the `Nil` case, we must
provide evidence that `Zero :>=: Zero`. That's easy, as `GTEZero` provides
that evidence.

In the `:>` case, we recur on `xs` and then we have to decide whether or not to
include `x` in the output. If `f x` holds, include `x` and use the proof on shorter
lists, `gte` to prove the greater-than relationship on larger lists, using `GTESucc`.
If `f x` does not hold, then we need to use `n :>=: m` to prove `Succ n :>=: m`. This
is because the input list is getting longer, but the output list is staying the same.
Our inductive proof / recursive function `gteSuccLeft` does the job for us.

Sometimes, we just want to be inclusive
---------------------------------------

By parameterizing `EVec` with a property that should hold of the hidden `n` parameter,
we can give a more precise type to `filter`. But we still sometimes want to write a
function like `toVec`, where *any* output length is acceptable. All we need to do is
define a universal property:

> data AlwaysTrue :: Nat -> Type where
>   Always :: AlwaysTrue n

This says that `Always` is a proof of `AlwaysTrue n`, for *any* `n`. Our original
`ExVec a` could now be written `EVec AlwaysTrue a`. And indeed we'll do that for `toVec`:

> toVec :: [a] -> EVec AlwaysTrue a
> toVec []     = EVec Always Nil
> toVec (x:xs) = case toVec xs of EVec _ xs' -> EVec Always (x :> xs')

It is perhaps annoying to use this `Always` constructor in this case, but this problem
can be mitigated with type synonyms and [pattern synonyms](http://cs.brynmawr.edu/~rae/papers/2016/patsyns/pattern-synonyms.pdf), but the details would bring us too far afield for now.
