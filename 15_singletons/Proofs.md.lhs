---
title: Proofs lecture notes
---

Proofs in types
===============

Here, we will continue to build out the `Vec` example, exhibiting the need
for ever fancier types. These notes are, again, a Literate Haskell file.
Here are our basic definitions:

> {-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving,
>              TypeFamilies, TypeOperators, AllowAmbiguousTypes, TypeApplications #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}

> module TypeFamilies where

> import Data.Kind  ( Type )
> import Data.Type.Equality ( (:~:)(..) )
> import Unsafe.Coerce      ( unsafeCoerce )

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat
>   deriving (Eq, Ord, Show)

> type family (a :: Nat) + (b :: Nat) :: Nat where
>   Zero   + b = b
>   Succ a + b = Succ (a + b)
> infixl 6 +

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

> deriving instance Show a => Show (Vec n a)

> -- singleton Nat
> data SNat :: Nat -> Type where
>   SZero :: SNat Zero
>   SSucc :: SNat n -> SNat (Succ n)
> deriving instance Show (SNat n)

We are *still* trying to get a more efficient version of `reverse` to
type-check. Here is what we'd like:

```haskell
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go :: Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil       = acc
    go acc (y :> ys) = go (y :> acc) ys
```

Compiling gives us these errors, in brief:

    • Could not deduce: (m + 'Zero) ~ m

    • Could not deduce: (m + 'Succ n1) ~ 'Succ (m + n1)

The problem is that our definition of `+`, above, pattern-matches
on its first argument. GHC has no way of knowing that the operation
is commutative and that `m + Zero` should indeed reduce to `m`. Instead,
we must *prove* these facts. To do so, we need to be able to write down
first-class type equalities.

Type equality
-------------

Haskell comes with two separate (but closely related) notions of type
equality. The first, simpler version is spelled `~` and can be used as
a constraint on a type, much like the way `Eq` is used. For example,
the following function requires that the sum of the lengths of its
two input vectors is two:

> getTwoElts :: ((m + n) ~ Succ (Succ Zero)) => Vec m a -> Vec n a -> (a, a)
> getTwoElts Nil             (x :> y :> Nil) = (x, y)
> getTwoElts (x :> Nil)      (y :> Nil)      = (x, y)
> getTwoElts (x :> y :> Nil) Nil             = (x, y)

The `~` constraint tells GHC that, no matter what choice we make for
`m` and `n`, their sum will be two. GHC can thus tell that the pattern
match written is exhaustive; we do not have to consider cases where more
elements are in either vector.

The other form of equality is a normal type (not a constraint), and is
spelled `:~:`. It can be imported from `Data.Type.Equality`, but its definition
is quite short:

```haskell
data a :~: b where
  Refl :: a :~: a
```

This type is a GADT (note how the type of the constructor constrains both type
variables to be the same) that expresses the equality between two types.
Pattern matching on an element of `:~:` will always reveal `Refl`. By the type
of `Refl`, this match will also tell GHC that the two type indices of `:~: are
equal in the body of the pattern match. For example:

> notty :: (a :~: Bool) -> a -> a
> notty Refl x = not x

That `Refl` pattern match tells GHC that `a` is `Bool`, and so we can treat
`x` of type `a` as a `Bool`.

At first blush, the need to pattern-match seems unfounded. After all, there can
only ever be one constructor of `:~:`, so we don't learn anything by pattern-matching,
right? Not quite. Witness this call to `notty`:

> silly :: Char
> silly = (error "I faked a proof!") 'x'

Because Haskell is lazy, it's possible that the argument of type `a :~: Bool` is
a computation that diverges (that is, doesn't reduce to a value). In this case,
we're just using `error` (recall `error`'s magical type: `String -> a`), but
we could also have written a function that contains infinite recursion. The
pattern-match on `Refl` forces evaluation of the argument to ensure that the
proof is valid. If you try to print out the value of `silly`, you'll get an
exception `"I faked a proof!"`. Which is exactly what we did.

Proofs about `Nat`s
-------------------

In order to get `reverseVec` to type-check, we'll need a proof that `m + Zero`
is `m`, for any `m`. In other words, we need this:

```haskell
plusZero :: (m + Zero) :~: m
```

That will indeed work, but it's impossible to implement. The problem is that
such a proof requires induction, and induction requires case analysis. GHC
just doesn't do automatic case analysis and induction in types. (Yet.) So
we need to guide it. The way to do this is to provide a GADT such that
pattern-matching on the GADT guides the induction. Understanding this in
the abstract is nigh impossible, so let's look at the final answer:

> plusZero :: SNat m -> (m + Zero) :~: m
> plusZero SZero      = Refl
> plusZero (SSucc m') = case plusZero m' of Refl -> Refl

We'll go through this slowly.

In the first equation, pattern matching on the input tells us that `m ~ Zero`,
according to the type of `SZero :: SNat Zero`. Thus, the right-hand side needs
to prove (that is, have the type of) that `(Zero + Zero) :~: Zero`. But our
first equation for `+` applies to the left-hand type there, so really we need
to prove that `Zero :~: Zero`. That's sure easy -- they're the same! So we
just use `Refl`, and GHC infers that we want that `Refl` to prove `Zero :~:
Zero`. The first case is all set.

In the second equation, we learn that `m ~ Succ m'` for some `m'`. (Again,
we have learned this by consulting the type for `SSucc`.) So, the right-hand
side needs to have type `(Succ m' + Zero) :~: Succ m'`. The left-hand side
here reduces to `Succ (m' + Zero)`. So we're left needing to find something
with type (that is, left needing to prove) `Succ (m' + Zero) :~: Succ m'`.
We could do this, if only we had a way of knowing that `(m' + Zero) :~: m'`.
But we do! Just use recursion. That's what the `plusZero m'` call does: it
produces something of type `(m' + Zero) :~: m'`. We pattern-match on this,
finding `Refl`, and then GHC can assume that `(m' + Zero) ~ m'`. With this
assumption, the type `(m + Zero) :~: m` (our original type of the result
of `plusZero`) reduces to `Succ m' :~: Succ m'`, which is surely easy to
prove with `Refl`. QED.

Note that we cannot just use `plusZero m'` as the right-hand side of the
equation. Even though `plusZero m'` is indeed `Refl`, it has the wrong
type! `plusZero m'` has type `(m' + Zero) :~: m'`, but we need something
of type `(Succ m' + Zero) :~: Succ m'`. We are thus left to pattern-match,
get out a `Refl`, and then produce another `Refl`.

Taking a step back, what have we done here? We've encoded a proof reasoned by
induction using a function defined by recursion. This is possible because
*induction and recursion are the same* (essentially). Think about it: when can
you use induction? When you can prove a base case and, assuming a fact is true
for smaller elements, you can prove it for larger elements. When can you use
recursion? When you can write a base case and, assuming the function works on
smaller elements, you can make it work on larger elements. The link here
between a technique of mathematical logic -- induction -- and a technique of
function programming -- recursion -- is not incidental. It forms a part of the
[Curry-Howard
correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).

Type arguments
--------------

The `plusZero` function is good for the first equation of `reverseVec`'s `go`,
but we need a further fact to get the second equation to type-check. We need
to know that `(m + Succ n) :~: Succ (m + n)`. Here is an attempt at the proof:

```haskell
plusSucc :: SNat m -> (m + Succ n) :~: Succ (m + n)
plusSucc SZero      = Refl
plusSucc (SSucc m') = case plusSucc m' of Refl -> Refl
```

But, alas, we get a type error:

    • Couldn't match type ‘m + 'Succ n0’ with ‘m + 'Succ n’
      Expected type: SNat m -> (m + 'Succ n) :~: 'Succ (m + n)
        Actual type: SNat m -> (m + 'Succ n0) :~: 'Succ (m + n0)
      NB: ‘+’ is a type function, and may not be injective
      The type variable ‘n0’ is ambiguous
    • In the ambiguity check for ‘plusSucc’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        plusSucc :: SNat m -> (m + Succ n) :~: Succ (m + n)

The problem is, essentially, that from a call `plusSucc m`, GHC has no way
to figure out exactly what we're trying to prove. Do we want
`(m + Succ Zero) :~: Succ (m + Zero)`? Do we want
`(m + Succ (Succ Zero)) :~: Succ (m + Succ Zero)`? Indeed, there are an infinite
family of facts that `plusSucc m` appears to prove. This situation is no good.

GHC detects this sad state of affairs just by looking at the type of `plusSucc`.
You can disable the check by specifying `AllowAmbiguousTypes` as a `LANGUAGE`
extension, as I have done in this file. Doing so allows the type to get through,
but then our use of `plusSucc` in the recursive case shows trouble: GHC still
doesn't know what we're trying to prove.

Once again, it's time just to reveal the answer:

> plusSucc :: forall n m. SNat m -> (m + Succ n) :~: Succ (m + n)
> plusSucc SZero      = Refl
> plusSucc (SSucc m') = case plusSucc @n m' of Refl -> Refl

The key observation here is that `plusSucc` does more than just take in
an `SNat` argument: it also takes *type arguments*. Every polymorphic Haskell
function takes type arguments. We usually just leave these out, as GHC can
infer them very easily. For example, here are two common library functions,
but with their type arguments more explicit:

```haskell
id :: forall a. a -> a
map :: forall a b. (a -> b) -> [a] -> [b]
```

We introduce type arguments with the `forall` keyword. Explicitly labeling
a type argument has two effects: with `ScopedTypeVariables` turned on, you
can then refer back to the named type variable in the body of the function;
and with `TypeApplications` turned on, you can pass type arguments explicitly.

We need both of these features to write `plusSucc`. First off, we choose the
type variable `n` to come first in `plusSucc`, even though `m` appears first.
This is because GHC can infer the choice of `m` from whatever we pass in as
the term-level argument of type `SNat m`. But GHC has no hope to infer our
choice of `n`. So we choose to put `n` first, making calls to `plusSucc`
more convenient. (Passing type arguments explicilty is always optional.
Here, we are omitting the second argument. If you want to omit an earlier
argument but supply a later one, use `@_` and GHC will infer the omitted
argument for you.)

In the recursive call to `plusSucc`, we want to prove that
`(m' + Succ n) :~: Succ (m' + n)`, so we pass in `n` as the choice for `n`
in the recursive call. This is done by the `@n` syntax introduced by
`TypeApplications`.

Getting `reverseVec` to type-check, finally
-------------------------------------------

The last piece to the puzzle is that we need a way of producing the right
`SNat`s to pass to `plusZero` and `plusSucc`. It turns out that this is easy
for `reverseVec`, as we can just build it as `go` recurs. Here is the final
version of `reverseVec`:

> reverseVec :: Vec n a -> Vec n a
> reverseVec xs = go SZero Nil xs
>   where
>     go :: SNat m -> Vec m a -> Vec p a -> Vec (m + p) a
>     go m acc Nil       = case plusZero m of Refl -> acc
>     go m acc (y :> (ys :: Vec p' a))
>       = case plusSucc @p' m of Refl -> go (SSucc m) (y :> acc) ys

The `Nil` case of `go` is easier to understand. We just want to prove that
`(m + Zero) ~ m`, easily done by matching on the result of `plusZero m`.

The `:>` case is harder. We need to know that `(m + Succ p') ~ Succ (m + p')`,
where `p'` is the length of `ys`. Our first problem is that we need to bind
the `p'` variable. Note that `p` doesn't work, as `p` is really `Succ p'`.
The way to bind a type variable that's not mentioned in a type signature is
to put a so-called pattern signature in a function's pattern. In this case,
we write `:: Vec p' a` as the type of `ys` in the pattern in the second
equation for `go`. This pattern signature binds `p'` as a new type variable.
(It also binds `a`, but we don't need that variable.) Then, we pass `p'` as
the type argument to `plusSucc`, and we are then allowed to recur.

What about the runtimes of `plusZero` and `plusSucc`?
-----------------------------------------------------

The goal of this line of inquiry was to make a function to reverse `Vec`s in
linear time. This is doable for lists, so we really should be able to do it
for `Vec`s. However, `reverseVec`, as written, does not achieve this goal.
At every recurrence, it calls `plusZero` or `plusSucc`, both of which are
*linear* in the value of `m`, which in turn is linear in the size of the
vector being reversed. So `reverseVec` is still quadratic! Oy.

One might wonder, at this point, why `plusZero` or `plusSucc` has to be
run at all. After all, these functions return values of type `:~:`, and there
is only one constructor of type `:~:`. Thus, can't we just jump to the chase
and assume that `plusZero` and `plusSucc` just return `:~:`? Not quite. The
problem is that `plusZero` or `plusSucc` might never return.

Haskell is a *partial* language, meaning that it admits "functions" that aren't
really functions, because they don't evaluate to anything (or don't return
on every input, like the standard library's `head` function). Because of this
possibility of non-termination, GHC can't just erase calls to `plusZero` and
`plusSucc`. Instead, it must run the functions simply to see if they terminate.

One day in the future, it is my hope that Haskell will have a termination checker.
(Don't worry -- I know this is impossible in general. But `plusZero` and `plusSucc`
are really easy to see that they terminate!) Until then, we can effectively assert
to GHC that they terminate.

The key observation here is that, if we know `plusZero` and `plusSucc` terminate,
then we never have to run them. We know they have to return `Refl` after all. So,
replacing the bodies of these two functions with `Refl` is tantamount to asserting
that they terminate. But we don't want to just delete the function bodies and write
`Refl`, because then GHC wouldn't be type-checking our proofs. We want to assert
termination, but nothing more than just that.

So we will use GHC's `RULES` mechanism. This feature was first described in a
[paper](https://www.haskell.org/haskell-symposium/2001/2001-62.pdf#page=209)
at the Haskell Symposium in 2001; it is also documented in the [GHC
manual](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#rewrite-rules).
The idea of `RULES` is that we provide an expression pattern and a substitution.
Whenever GHC spots something that matches the pattern, it substitutes in the right-hand
side substitution. It's that simple. GHC checks to make sure that the pattern and
the substitution have the same types, but no further checks are done. This feature
is thus very unsafe but very powerful.

In our case, here are the rules we want:

> {-# RULES
> "plusZero" forall m. plusZero m = unsafeCoerce (Refl @())
> "plusSucc" forall m. plusSucc m = unsafeCoerce (Refl @())
> #-}

The funny syntax arises because `RULES` are not meant to change a program's
meaning. They should just act as an optimization. Thus, they are considered to
be not part of Haskell proper, but instead are a feature called a
[pragma](https://en.wikipedia.org/wiki/Directive_(programming)).

Every rule has a name, a list of variables mentioned on the left-hand side,
and then an equation. When GHC spots something that matches the left-hand side,
it inserts the right-hand side. In our case, we want to insert `Refl`. But just
writing `Refl` causes GHC to complain, because GHC can't prove that, say,
`(m + Zero) ~ m`. (If it could, we wouldn't need the proofs!) so we use `unsafeCoerce`,
which does exactly what it says. `unsafeCoerce` (imported from the module
`Unsafe.Coerce`) has the type `a -> b`. It can take any time and transmute it to
have another type. At runtime, `unsafeCoerce` does nothing at all -- it's erased
during compilation. In other words, it's perfect for us here.

There are two more details to cover. The first is the `@{}` argument to `Refl`.
A bug in GHC 8.0.x (but fixed for GHC 8.2) causes GHC to warn about some unbound
`k`. (The `k` arises from `Refl`'s type: `forall k (a :: k). a :~: a`. The types
related by `Refl` can have any kind `k`.) By choosing to instantiate `Refl` at
the unit type `()`, we avoid the problem.

The last detail is that we need to disable inlining of `plusZero` and `plusSucc`.
GHC's optimizer is very agressive about [inlining](https://en.wikipedia.org/wiki/Inline_expansion),
which means to replace a function call with the function body in order to speed
up execution. If that happened for `plusZero` or `plusSucc`, our rules might never
match. So we disable inlining for these functions with another pair of pragmas.
(If we forget these, GHC helpfully suggests to add them.)

> {-# NOINLINE plusZero #-}
> {-# NOINLINE plusSucc #-}

Now, we've really done it: we've written that pesky linear-time `reverseVec` and
learned quite a bit along the way.

A note on the complexity of fancy types
---------------------------------------

It is easy to wonder at this stage: is any of this worth it? Working with fancy
types is hard, requiring a deep set of knowledge of Haskell's type system and lots
of time staring down type errors. These are real costs. Should we pay them?

My answer: sometimes. It is surely impractical to use fancy types in every application
everywhere. (I hope that support for, and general understanding of, fancy types in
the future makes this more practical. But we're not there yet.) But maybe in the
deep dark heart of your application, some algorithm is very important and yet
very easy to get wrong. Adding fancy types there might just save your skin. Though
we haven't seen how yet, it is possible to call a function with fancy types from
functions with ordinary types, and so you could make just that one really important
part precisely typed while leaving the rest of your application alone. In this
use case, I think the fancy types surely are worth it.
