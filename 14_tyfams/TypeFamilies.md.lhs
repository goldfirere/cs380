---
title: Type families lecture notes
---

Computing in Types
==================

This class builds directly upon the work of last class, where we ended
pondering a `reverse` function on `Vec`s. To set the scene (and to keep
this Literate Haskell file self-contained), we'll start with these
definitions:

> {-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving,
>              TypeFamilies, TypeOperators #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}

> module TypeFamilies where

> import Data.Kind  ( Type )
> import Prelude hiding ( reverse, (++), replicate )

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

> deriving instance Show a => Show (Vec n a)

> snoc :: Vec n a -> a -> Vec (Succ n) a
> snoc Nil       x = x :> Nil
> snoc (y :> ys) x = y :> snoc ys x

> reverse :: Vec n a -> Vec n a
> reverse Nil       = Nil
> reverse (x :> xs) = snoc (reverse xs) x

A better reverse
----------------

This definition of `reverse` type-checks and has the correct runtime behavior.
But it's *quadratic* in the length of the input list. To see why, we can
observe that `snoc` runs in linear time, traversing the entire list in order
to change the last element. We can further see that `reverse` calls `snoc` for
every element. Thus: `reverse` is quadratic. But reversing a linked list
shouldn't take quadratic time -- it should work in *linear* time. And, indeed,
we can do this quite easily on lists:

> reverseList :: [a] -> [a]
> reverseList xs = go [] xs
>   where
>     go acc []     = acc
>     go acc (y:ys) = go (y:acc) ys

The idea here is that we use an helper function `go` with an accumulating
parameter `acc`. At every recurrence of `go`, we simply append to the
accumulator. When we reach the end of the input list (the second argument
to `go`), we've added all the elements, conveniently in reverse order.

But this fails miserably when you try it with `Vec`s:

```haskell
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go acc Nil       = acc
    go acc (y :> ys) = go (y :> acc) ys
```

Trying to compile this leads to several errors, and fixing all the errors
will lead us to several new realizations, powering this lecture and the
next.

Untouchable variables
---------------------

Here is a snippet of the first error:

    • Couldn't match expected type ‘t’ with actual type ‘t1’
        ‘t’ is untouchable

"Untouchable"? What does that mean? Section 5.2 of [this
paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf)
on GHC's type inference algorithm will tell you. (Section 5, up through 5.2, is
actually quite accessible. I'm not joking when I link to the academic paper!)
However, I can summarize: an *untouchable* variable is a type variable
that GHC is unable to infer from the code written. This error almost always
arises from a pattern match over a GADT that does not have a type signature.
Indeed, our `go` helper function does a GADT pattern match, but `go` does not
have a type signature, leading to this error.

(You can also cause this error to happen if you leave off the type signature
on, say, `snoc`. Try it!)

The good news about *untouchable* errors is that they are generally straightforward
to fix: just add a type signature. But, that causes a fresh problem: what type does
`go` have, anyway? It turns out that we don't yet have enough machinery to answer
such a question. Before we tackle something as hard as `go`, let's start with
something simpler.

Concatenating `Vec`s
--------------------

The Haskell Prelude comes with the `(++)` operator on lists:

```haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

Translating the function definition to `Vec`s is easy:

> Nil       ++ ys = ys
> (x :> xs) ++ ys = x :> (xs ++ ys)
> infixr 5 ++

Of course, writing this function without a type signature
leads to an *untouchable* error. So we must write a type. Seems simple enough:
the function takes two `Vec`s and outputs a third:

```haskell
(++) :: Vec n a -> Vec m a -> Vec ?????? a
```

The problem, of course, is that the result length is neither `n` nor `m`, the
two input lengths. Instead, it must be the *sum* of `n` and `m`. We can't simply
write `+`, though, because we are working in a *type*, and the `+` that we know
and love is an expression, not a type. Instead, we must define the `+` operation
to work on type-level numbers, using a *type family*. I'll write this type family
in two ways to demonstrate:

> type family Plus (a :: Nat) (b :: Nat) :: Nat where
>   Plus Zero     b = b
>   Plus (Succ a) b = Succ (Plus a b)

> type family a + b where
>   Zero   + b = b
>   Succ a + b = Succ (a + b)
> infixl 6 +

Type families are essentially functions on types. (I say "essentially" because
I think the current design of type families in Haskell is [a bit
wrong](http://cs.brynmawr.edu/~rae/papers/2017/partiality/partiality.pdf).)
They are defined by equations that control the compile-time evaluation of the
type families. So, when we say `Plus Zero (Succ Zero)` in a type, that is
equivalent to `Succ Zero`, according to the first equation. (You can see this
in GHCi by typing `:kind! Plus Zero (Succ Zero)`. Note the `!`, which causes
GHCi to try to evaluate any type families in a type.)

The first definition above uses an alphanumeric name, `Plus`. Because this is
a type, the name of the type must be written with an initial capital letter.
This definition also gives the kinds of the two arguments and the result
(in this case, all `Nat`, but there is no need for these to be the same).

The second definition uses a symbolic name, which can be any symbol, and omits
the kind signature. GHC can use kind inference to figure it all out for you.
There is no support for a standalone kind signature for type families the way
there is for ordinary functions. Also, because the type-level `+` is fully
unrelated to the ordinary `+`, we must give a fixity directive `infixl 6 +`
to get the right precedence and associativity for type-level addition.

Now that we have these type families in hand, we can write the type signature
for `(++)`:

> (++) :: Vec n a -> Vec m a -> Vec (n + m) a

Let's walk through how the definition of `(++)`, above, matches this type.

In the `Nil` case, we learn that `n` is `Zero`. Thus, the output, `ys`, should be
of type `Vec (Zero + m) a`. But by the definition of `(+)`, we see that
`Zero + m` is the same as `m`. So the output type is `Vec m a`, conveniently the
type of `ys`.

In the `:>` case, we learn that `n` is `Succ p` for some `p`. The output type
is now `Vec (Succ p + m) a`. But by the definition of `(+)`, we see that
`Succ p + m` is `Succ (p + m)`, so that the output type is
`Vec (Succ (p + m)) a`. The output expression is `x :> (xs ++ ys)`, where
`xs :: Vec p a` and `ys :: Vec m a`. By the type of `(++)`, we see that
`xs ++ ys :: Vec (p + m) a`, and thus the type of `x :> (xs ++ ys)` is
`Vec (Succ (p + m)) a`, exactly what we want. Huzzah!

GHC does not know how to add
----------------------------

Let's now reconsider the type of the `go` function in `reverseVec`:

```haskell
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go acc Nil       = acc
    go acc (y :> ys) = go (y :> acc) ys
```

Spend a few moments thinking about what the type of that function is.
Another way of thinking about this is: what are the invariants about the
lengths of three vectors involved in `go` (I'm thinking about the two
input vectors and the one output here)? Of course, because `go` is a
recursive function, thinking about this is akin to thinking about loop
invariants in an imperative program.

Because `go` takes an element from the second input and puts it on the first,
we can figure out that the type of `go` should be the same as the type of
`(++)`: the output length is the sum of the input lengths. But note that the
function is *not* the same: `go` reverses, while `(++)` does not.

So, let's try this:

```haskell
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go :: Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil       = acc
    go acc (y :> ys) = go (y :> acc) ys
```

This gives us grief, though. The grief, in brief, is:

    • Could not deduce: (m + 'Zero) ~ m

    • Could not deduce: (m + 'Succ n1) ~ 'Succ (m + n1)

The problem is that GHC does not know how to add. These facts are plainly
true of addition, but it's not obvious to GHC from the definition of `(+)`.
Indeed, when we considered arithmetic on `Nat`s, we had to *prove* these
facts using induction. And, so, we will have to do write these proofs in
Haskell in order for `reverseVec` to type-check. Writing proofs in Haskell
requires singleton types, as we will see. So, let's first explore singleton
types, and then we'll return, once again, to `reverseVec`.

Singletons
----------

A simpler motivation for singletons comes from a desire to translate yet
another common list function, `replicate`:

```haskell
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x
```

A call to `replicate n x` makes a list containing `n` copies of `x`. Translating
this definition to work over `Vec`s is unsurprising... but the *type* is problematic.
Consider this first draft:

```haskell
replicate :: Int -> a -> Vec n a
```

The problem here is that the `n` in the output type is utterly unrelated to the
input `Int`. That's clearly wrong. A second problem is that the input number
isn't really an *integer*. It should be a natural number. Of course, updating the
type to

```haskell
replicate :: Nat -> a -> Vec n a
```

doesn't really get us much farther. What we need is a way of connecting the
term-level, runtime natural number to a type-level, compile-time natural number.
A *singleton type* does this for us. Here is the definition:

> data SNat :: Nat -> Type where
>   SZero :: SNat Zero
>   SSucc :: SNat n -> SNat (Succ n)

This is called a singleton type (or, more accurately, a family of singleton
types... but I won't be that accurate) because, for every index to `SNat`,
there is exactly one inhabitant (ignoring the possibility of infinite
recursion or `undefined` or other sort of cheating). That is, `SNat Zero` has
exactly one inhabitant: `SZero`. `SNat (Succ (Succ Zero))` has exactly one
inhabitant: `SSucc (SSucc SZero)`.

Theorem: For every `n`, `SNat n` has exactly one inhabitant.

Proof: By induction on `n`.

 * Case `n = Zero`: `SZero` inhabits `SNat Zero`. Any other inhabitant would
   need to start with one or more `SSucc`s, but any use of `SSucc` would lead
   to a type index that starts with `Succ`, and `Succ` $\neq$ `Zero`.

 * Case `n = Succ n'`: The induction hypothesis says that there is exactly
   one inhabitant of `SNat n'`. Let's call this `x`. We can see that `SSucc x`
   is an inhabitant of `SNat n` (that is, `SNat (Succ n')`). Are there others?

     * One possibility is that `SZero` inhabits `SNat (Succ n')`. But this is
       impossible because `SZero`'s index is `Zero`, and `Zero` $\neq$ `Succ`.

     * Another possibility is that a sequence of $m$ `SSucc`s followed by an
       `SZero` (where $m>0$) inhabits `SNat (Succ n')`. But in this case,
       removing one `SSucc` from the sequence would be an inhabitant of `SNat
       n'`. Call this `y`. Either `y` equals `x` (in which case we have not
       found an inhabitant distinct from `SSucc x`) or it does not (in which
       case we have a contradiction with our induction hypothesis). Either way,
       our theorem is proved.

QED.

The close correspondence between the term-level value (created with `SZero`
and `SSucc`) and the type-level index (created with `Zero` and `Succ`) means that
the value and the type are isomorphic. Indeed, we can consider them to be
equal, for the right definition of equality.

In practical terms, this means that an argument of type `SNat n` means that a
function can use `n` at runtime *and* at compile-time. The runtime version is the
inhabitant of `SNat n` and the compile-time version is just `n`. But these are
always the same, so we need not consider them separately.

Perhaps going back to the example will make this all clearer. Here is the type
(and body) of `replicate`:

> replicate :: SNat n -> a -> Vec n a
> replicate SZero      _ = Nil
> replicate (SSucc n') x = x :> replicate n' x

The `n` is used in the type because it is the index to the output type `Vec n a`.
It is used in the term because we must pattern match on choice of `n` to determine
how long to make the output list.

Singleton types are a way of faking *dependent types* in Haskell. There are
several dependently typed languages available ([Coq](https://coq.inria.fr/),
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), [Idris](http://www.idris-lang.org/),
and [F*](https://www.fstar-lang.org/) come to mind), but none of these, to my knowledge,
is currently used in business-oriented production software. Singleton types are not
necessary in a dependently typed language, which can just use the same `n` in both term
and type. But Haskell does not yet have dependent types, and so we must use singletons.
On the other hand, Haskell is a production-ready language, so it has that going for it.

Singleton types in Haskell are explored in a [paper](http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf)
of mine, though I did not invent the technique. See the paper for links to prior work.
Adding dependent types to Haskell is my main, overarching research project for the past
and future several years; my [thesis](http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf)
is dedicated to the subject.

Singleton types will be key in getting our more efficient version of `reverse` to type-check,
but the details will have to wait for another day. In the meantime, singletons are
useful in translating several functions from lists to `Vec`s, such as `replicate` (as we've
seen), `take`, and `drop`. Writing these last two would be good exercises at this point.
