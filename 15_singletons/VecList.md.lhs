---
title: VecList lecture notes
---

Lists of vectors
================

As usual, this is a Literate Haskell file, with the obligatory header:

> {-# LANGUAGE StandaloneDeriving, TypeInType, TypeOperators, GADTs,
>              TypeFamilies, UndecidableInstances #-}

> module VecList where

> import Data.Kind ( Type )
> import Prelude hiding ( concat, (++) )

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

> type family (a :: Nat) + (b :: Nat) :: Nat where
>   Zero   + b = b
>   Succ a + b = Succ (a + b)
> infixl 6 +

> type family (a :: Nat) * (b :: Nat) :: Nat where
>   Zero   * b = Zero
>   Succ a * b = b + (a * b)
> infixl 7 *

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

> deriving instance Show a => Show (Vec n a)

> (++) :: Vec n a -> Vec m a -> Vec (n + m) a
> Nil       ++ ys = ys
> (x :> xs) ++ ys = x :> (xs ++ ys)
> infixr 5 ++

Consider this standard-library function:

```haskell
concat :: [[a]] -> [a]
concat []         = []
concat (xs : xss) = xs ++ concat xss
```

This function takes a list of lists and flattens it to just one list. So
`concat [[1,2], [3], [4, 5, 6]]` is `[1,2,3,4,5,6]`.

Here's how we might try to write it over `Vec`s:

> concatRect :: Vec m (Vec n a) -> Vec (m * n) a
> concatRect Nil         = Nil
> concatRect (xs :> xss) = xs ++ concatRect xss

Note that we're using type-level multiplication now, with `*`. This type
family is defined above, in the introduction. (Its definition requires
`UndecidableInstances`. That flag disables GHC's very simplistic termination
checker for type families. Enabling the extension means that GHC might not
terminate when trying to compile your file. But that's a risk we'll have to
take.)

The problem with `concatRect` is that it's limiting. It allows us only
to concatenate a *rectangular* arrangement of elements, where each `Vec`
stored within the larger `Vec` has the same length `n`. This function is
thus inapplicable to our initial example with the numbers 1 through 6.

In order to contemplate a more general `concat`, we need a new structure, for
holding lists of `Vec`s of uneven lengths. But we still want the structure's
type to record their lengths, so that we can sum all the lengths in the type
of `concat`. To do this, we'll need type-level lists:

> data VecList :: [Nat] -> Type -> Type where
>   VLNil :: VecList '[] a
>   (:>>) :: Vec n a -> VecList ns a -> VecList (n ': ns) a
> infixr 5 :>>

The `VecList` type is indexed by a type-level *list* of type-level `Nat`s.
Each element in the list is the index for one of the `Vec`s stored in the
list. (All elements have the same type `a`.) In the `VLNil` empty list
case, we see that the type-level list of lengths is empty. (GHC requires
that the promoted nil constructor and the promoted cons constructor are *always*
written with their preceding ticks, as we see above.) In the `:>>` case,
we get a `Vec n a`, a `VecList ns a` (that is, a `VecList` indexed by the type-level
list of type-level `Nat`s, which we're calling `ns`), producing a
`VecList (n ': ns) a`. That is, the result type just conses the new `n` onto
the list `ns`.

We can now write the following translation of our original example:

> stuffs = (1 :> 2 :> Nil) :>> (3 :> Nil) :>> (4 :> 5 :> 6 :> Nil) :>> VLNil

GHCi can tell us

    λ> :t stuffs
    stuffs
      :: VecList
           '['Succ ('Succ 'Zero), 'Succ 'Zero, 'Succ ('Succ ('Succ 'Zero))]
           Integer

That should be no surprise. The index to the type of `stuffs` is `'[2,1,3]`, corresponding
to the lengths of the constituent elements of `stuffs`.

Writing `concat` is now relatively easy:

> type family Sum (ns :: [Nat]) :: Nat where
>   Sum '[]       = Zero
>   Sum (n ': ns) = n + Sum ns

> concat :: VecList ns a -> Vec (Sum ns) a
> concat VLNil        = Nil
> concat (xs :>> xss) = xs ++ concat xss

Note that, in the type of `concat`, we must use a new type family `Sum` that sums up all
the elements in `ns`. It would be ill-kinded to say `Vec ns a`, because `Vec` is indexed by
a `Nat`, but `ns` is a `[Nat]`.
