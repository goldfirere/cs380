---
title: Fin lecture notes
---

`Fin`: A type with a known number of inhabitants
================================================

The `Data.List` module gives us the following function, used to index into
a list:

```haskell
(!!) :: [a] -> Int -> a
_        !! n | n < 0 = error "negative index"
(x : _)  !! 0 = x
(_ : xs) !! n = xs !! (n-1)
[]       !! _ = error "index too large"
```

There are several problems with this function:

1. It is inefficient; computing `list !! n` takes `n` steps. If you have to index
into a list, a list is probably the wrong data structure.

2. It is partial: the function fails on negative numbers, yet the type allows negative
numbers to be provided.

3. It is partial: the function fails on indices that are too large, yet the type allows
these numbers to be provided.

We can't do much about (1). But let's fix (2) and (3). We could handle (2) easily by
swapping out the `Int` for a `Nat`. Fixing (3), on the other hand, requires switching
to `Vec`, of course.

Indexing into `Vec`
-------------------

It's high time we had our obligatory Literate Haskell header:

> {-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, EmptyCase #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}

> module Fin where

> import Data.Kind ( Type )
> import Prelude hiding ( (!!) )

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

> deriving instance Show a => Show (Vec n a)

Here might a first stab at giving a type to an indexing operator over `Vec`:

```haskell
(!!) :: Vec n a -> Nat -> a
```

Even without writing the definition, we can see problems lurking ahead. This
type cannot rule out problem (3), as there's nothing constraining `Nat`. Some
might be tempted by something like

```haskell
(!!) :: Vec n a -> SNat n -> a
```

but that, too, is wrong: the index is always exactly the length of the list,
so this will fail every time. What we need is a type that contains exactly
`n` inhabitants (that is, a type such that there are `n` values of that type).
We can then order those elements and use that to index into a `Vec n a`.

The `Fin` type
--------------

The `Fin` type is exactly what we want:

> data Fin :: Nat -> Type where
>   FZero :: Fin (Succ n)
>   FSucc :: Fin n -> Fin (Succ n)

This type can be hard to understand, so be patient and let its true meaning arrive
to you slowly.

Theorem: For every natural number `n`, `Fin n` has `n` inhabitants.

Proof: By induction on `n`.

Base case, `n == Zero`: We can see that `Fin Zero` is uninhabited, because both
constructors of `Fin` produce a `Fin` whose index is a `Succ` of something, not
`Zero`.

Inductive case, `n == Succ n'`: Here, we assume that `Fin n'` has exactly `n'`
inhabitants. We must prove that `Fin n` -- that is, ` Fin (Succ n')` -- has `n' + 1`
inhabitants. Well, how can we create something of type `Fin (Succ n')`? We can
use `FZero`; that gives us one inhabitant. Or, for every inhabitant of `Fin n'`, we
can apply `FSucc` to it; that gives us `n'` inhabitants. Voilà! We have `n' + 1`
inhabitants, as desired. QED.

In other words, for every non-zero number `n`, we have as many inhabitants as
`n - 1` has, plus one extra for `FZero`. Part of the magic here is that
`FZero` has many different types: we have `FZero :: Fin (Succ Zero)` and
`FZero :: Fin (Succ (Succ (Succ Zero)))`, for example. Note that we do *not*
have `FZero :: Fin Zero`, as `Fin Zero` is an empty type. (It has `Zero`
inhabitants, of course.) But otherwise, `FZero` can have a type with any
non-zero index. We'll return to this phenomenon as we consider `(!!)` below.

Indexing into `Vec` with `Fin`
------------------------------

Now that we have the right type with which to express our index, we can write
the indexing operation, which I'll call `get`:

> get :: Fin n -> Vec n a -> a
> get FZero     (x :> _)  = x
> get (FSucc f) (_ :> xs) = get f xs

No more partiality. Huzzah!

Let's see some examples of this in action:

> stuff = 5 :> 3 :> 8 :> Nil

> five  = get FZero stuff
> three = get (FSucc FZero) stuff
> eight = get (FSucc (FSucc FZero)) stuff
> -- bad = get (FSucc (FSucc (FSucc FZero))) stuff

The first three compile and work, as expected. GHC can infer the type of `stuff`
to be `Vec 3 Integer` (I'll use normal Arabic numbers for brevity and clarity).
(The fact that GHC infers `Integer` is because of the [dreaded monomorphism
restriction](http://stackoverflow.com/questions/32496864/what-is-the-monomorphism-restriction),
which causes some types to be defaulted. GHC chooses `Integer` as the default
choice for a type that is in the `Num` class.) GHC then uses the fact that the
length is 3 to infer the index of the `Fin` argument, taking advantage of `get`'s
type signature and that the `n`s are the same.

For `five`, GHC sees that `FZero` should have type `Fin 3`. The declaration
for `FZero` gives its type as `Fin (Succ n)`. So, is there an `n` such that
`Succ n` is `3`? Sure there is: choose `n` to be `2`. Accordingly, `FZero` (in
the line for `five`) is given the type `Fin 3` and all is well.

For `three`, GHC sees that `FSucc FZero` should have type `Fin 3`. The declaration
for `FSucc` gives its type as `Fin n -> Fin (Succ n)`. So, if we can give `FZero`
the type `Fin 2`, then `FSucc FZero` can have the type `Fin 3`. Looking at `FZero :: Fin 2`,
this type checks because `2` is not `Zero`. All is well.

For `eight`, GHC sees that `FSucc (FSucc FZero)` should have type `Fin 3`. Eventually,
it then decides that `FZero` must have type `Fin 1`, and this too works out.

But for `bad`, we get this error:

    • Couldn't match type ‘'Zero’ with ‘'Succ n0’
      Expected type: Vec ('Succ ('Succ ('Succ ('Succ n0)))) Integer
        Actual type: Vec ('Succ ('Succ ('Succ 'Zero))) Integer
    • In the second argument of ‘get’, namely ‘stuff’
      In the expression: get (FSucc (FSucc (FSucc FZero))) stuff
      In an equation for ‘bad’:
          bad = get (FSucc (FSucc (FSucc FZero))) stuff


That's because the process above will eventually lead to GHC requiring `FZero`
to have type `Fin Zero`. But, as argued above, this is impossible. So type-checking
fails, exactly as it should.

Laziness bites
--------------

You're probably wondering, after all the warmup around `(!!)`, why we took an unexpected
turn and defined `get` instead. The reason is that `get` has its arguments in the reverse
order of `(!!)`, and that unexpectedly makes a difference here. Let's explore why.

When I define

> (!!) :: Vec n a -> Fin n -> a
> (x :> _)  !! FZero   = x
> (_ :> xs) !! FSucc f = xs !! f

I get this warning:

    Pattern match(es) are non-exhaustive
    In an equation for ‘!!’: Patterns not matched: Nil _

It's quite subtle to see why `(!!)` should get this warning but `get` shouldn't. It
all has to do with laziness. The first step toward understanding here is to figure out
why `Nil` should be excluded in the first place: it's only excluded because `Fin Zero`
is uninhabited. Another way of looking at this is that pattern-matching on `Fin n` will
tell us that `n ~ Succ n'` (for some `n'`) no matter what constructor is matched against
the `Fin n`. Once we know that `n ~ Succ n'`, then `Nil` is excluded, as `Nil` requires
`n ~ Zero`. However, in `(!!)`, we pattern-match on the `Vec` *before* we pattern-match
on the `Fin`. At this point, we have no reason to exclude `Nil`.

How can we call `(!!)` with `Nil` though? Clearly, if we build the `Fin` with either
available constructor, we'll get an error. For example, trying `Nil !! FZero` produces

    • Couldn't match type ‘'Succ n0’ with ‘'Zero’
      Expected type: Fin 'Zero
        Actual type: Fin ('Succ n0)
    • In the second argument of ‘(!!)’, namely ‘FZero’

The only way to do this is if we supply a divergent term for the `Fin n`. Because
Haskell is a lazy language, this is indeed possible: When I type `Nil !! error "urk"`
into GHCi, I get

    *** Exception: Fin.md.lhs:(180,3)-(181,32): Non-exhaustive patterns in function !!

So we really have found our way to the impossible case. This problem doesn't happen
in `get` because GHC matches left-to-right. Trying `get (error "urk") Nil` yields

    *** Exception: urk

The faked `Fin n` gets evaluated in the process of matching to see whether it's
`FZero` or `FSucc`. The error we get is the one we should indeed get: the caller of
`get` lied, and so the lie is being thrown back in the user's face. Contrast to
the behavior of `(!!)`, where the same lie is ignore and instead it looks like the
problem is `(!!)`'s fault.

There are several solutions to this problem. We could reorder the arguments.
We could also use an explicit `case` to pattern-match. (See below.) Or, we could
simply prove that `Fin Zero`s don't exist and use this proof to complete the definition
of `(!!)`:

> Nil !! fin_zero = case fin_zero of {}

This line is actually the last defining equation for `(!!)`, above. (We
can't have these notes cause a warning upon loading, can we?) The body of the equation
is an [empty `case` expression](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#empty-case-alternatives). These `case` expressions evaluate the bit
between the `case` and the `of` (called the "scrutinee" in the functional programming
language design world). But there are no possible constructors that the scrutinee could
equal, so we leave the alternatives empty. We could actually omit the `{}` empty alternatives,
but that looks awfully anticlimactic.

(*Note*: Any time Haskell uses layout to determine, well, anything, you could also use
braces and semicolons. So you could say `seven = let { a = 3; b = 4 } in a + b`. Few
people do this, because then Haskellers would have to reduce how smug they act around
C and Java programmers. In the `case` above, leaving out the braces would leave Haskell's
layout rules to figure out what's going on. But the next line of the line will be indented
at the same level as the line above, and so GHC decides that there can be no lines included
as part of the `case`.)

(*Note*: It is important that an empty `case` evaluate the scrutinee. Most `case` expressions
evaluate the scrutinee only if necessary for pattern matching. But in an empty case, the
whole point is that there is nothing to pattern-match against, so we can't make our
desire to evaluate obvious. Thus, GHC has a special rule saying that empty `case` evaluates
the scrutinee.)

The type of a `case` expression is the type of all of its right-hand sides. But an
empty `case` has no right-hand sides, so it can have *any* type. In our case, it has
type `a`, and all is well.

For completeness, here is the way to implement `(!!)` by use a `case` to reorder the
pattern-match:

> (!!!) :: Vec n a -> Fin n -> a
> vec !!! fin = case (fin, vec) of
>   (FZero,   x :> _)  -> x
>   (FSucc f, _ :> xs) -> xs !!! f

Now that we have `Fin`, you can write functions like `findIndex` to return `Fin`s,
thus ensuring that the use of these indices will succeed.
