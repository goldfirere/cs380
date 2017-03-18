---
title: GADTs lecture notes
---

GADTs
=====

This file is a Literate Haskell file. If you download it in LHS format from
the website, you will be able to compile it in GHCi. When a file ends in the
extension .lhs, GHC looks for lines beginning with > and compiles only those.
Everything else is treated as a comment.

(This file is also in Markdown format for easy rendering to HTML, using
Pandoc.)

As we've seen a few times, Haskell supports *extensions*, enabling you to turn
on certain language features if you want them. From here on out, we'll be
using a bunch.

> {-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}  -- warn if we forget a case

> module Class13 where

> import Data.Kind  ( Type )
> import Prelude hiding ( reverse )  -- we'll define our own

You are Haskellers
------------------

At this point in the course, you know enough to consider yourself Haskellers.
While we have not discussed monads at any length, there is a ton of material
out there to help you. (Starting with Real World Haskell is a good bet.)
Instead, I want to focus the rest of the semester on material you cannot get
elsewhere. But it's high time I introduce you to a variety of resources you
might want.

 * Mailing lists: Haskell lives in a number of mailing lists, listed
    [here](https://wiki.haskell.org/Mailing_lists). I'm active on
    haskell-cafe.

 * IRC (Internet Relay Chat) is another great place to meet Haskellers. [This
   page](https://wiki.haskell.org/IRC_channel) has a nice listing. Though not
   listed there, I'm occasionally spotted in the #ghc channel on freenode.

 * The [Haskell subreddit](https://www.reddit.com/r/haskell/) is very active.
   I'm spotted there from time to time.

 * [Planet Haskell](http://planet.haskell.org/) is a blog aggregator about
   Haskell. There's some very nice reading there.

 * There is a growing number of companies that are interested in Haskell
   programmers, should you ever be looking for a job. These three have
   specifically asked me to recruit for them:

     - [Awake Networks](http://www.awakenetworks.com/): I have consulted for
       them -- a great team.

     - [Takt](http://takt.com/): These are Haskell True Believers, eager to
       grow their ranks. I have contacts at the company.

     - [AlphaSheets](http://www.alphasheets.com/): I don't have a personal
       relationship here, but they emailed me, looking for you.

   Beyond these companies I'll also put in a plug for [Jane
   Street](https://www.janestreet.com/), a financial services company that
   uses OCaml (very, very similar to Haskell); and
   [Facebook](https://www.facebook.com/), who are expanding their recent
   experiements in using Haskell to deliver some of their services.
   [Functional Jobs](https://functionaljobs.com/) is a clearinghouse for job
   postings from companies looking to hire people like you.

 * GHC is an open-source project, hosted
   [here](https://ghc.haskell.org/trac/ghc). I'm very active on the site. If
   you see a bug, post a ticket!

 * Want to change the language? Make a
   [proposal](https://github.com/ghc-proposals/ghc-proposals).

 * [Some](https://xkcd.com/1312/) [funny](https://ro-che.info/ccc/1)
   [pages](http://thecodelesscode.com/case/143)
   [about](http://i.imgur.com/6FhL6QJ.jpg)
   [Haskell](https://www.youtube.com/watch?v=Ci48kqp11F8). Notes: Randall
   Munroe writes xkcd; his sidekick, davean, is active in the Haskell
   community. [Ranjit Jhala](https://ranjitjhala.github.io/) is the star in
   the YouTube video (last link) and is very well-respected in the Haskell
   research community.

As a rule, the Haskell community is welcoming, mostly because we've all seen
how awesome Haskell is and we want to share the love. I got into Haskell
because it's pure, functional, and has fancy types. I've stayed because it's a
wonderful place to hang out.

Generalized Algebraic Datatypes (GADTs)
---------------------------------------

Consider the lowly `Maybe` type:

```haskell
data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a
```

When we declare the `Nothing` and `Just` constructors, we give them types that
end in `Maybe a`, the datatype we are declaring. But, what if the parameter to
`Maybe` there isn't `a`?

> data G a where
>   MkGInt  :: Int  -> G Int
>   MkGBool :: Bool -> G Bool

> foo :: G a -> a
> foo (MkGInt n)  = n + 5
> foo (MkGBool b) = not b

> bar :: forall a. G a -> a -> Bool
> bar (MkGInt _)  x = ((x :: a) :: Int) > 0
> bar (MkGBool _) x = x

Note that `x` has type `a` here. After the pattern-match, though, we know what
`a` is and can use that fact to produce a `Bool`.

We can now make a definition usable to represent a Haskell type:

> data TypeRep a where
>   TInt    :: TypeRep Int
>   TBool   :: TypeRep Bool
>   TDouble :: TypeRep Double
>   TMaybe  :: TypeRep a -> TypeRep (Maybe a)
>   TFun    :: TypeRep a -> TypeRep b -> TypeRep (a -> b)

> zero :: TypeRep a -> a   -- produces a "zero" of a certain type
> zero TInt       = 0
> zero TBool      = False
> zero TDouble    = 0.0
> zero (TMaybe _) = Nothing
> zero (TFun _ b) = \ _ -> zero b

OPTIONAL: If you really want to blow your mind, you can do the example above
a bit more polymorphically:

> data PolyRep (a :: k) where
>   PInt    :: PolyRep Int
>   PBool   :: PolyRep Bool
>   PDouble :: PolyRep Double
>   PMaybe  :: PolyRep Maybe
>   PFun    :: PolyRep (->)
>   PApp    :: PolyRep a -> PolyRep b -> PolyRep (a b)

> pzero :: PolyRep a -> a
> pzero PInt                         = 0
> pzero PBool                        = False
> pzero PDouble                      = 0.0
> pzero (PApp PMaybe _)              = Nothing
> pzero (PApp (PApp PFun _) p)       = \_ -> pzero p
> pzero (PApp (PApp (PApp p _) _) _) = absurd p

> -- This is a little inductive "proof" that there are no representations
> -- of ternary (or more) operators
> absurd :: forall (a :: k1 -> k2 -> k3 -> k4) b. PolyRep a -> b
> absurd (PApp p _) = absurd p

END OPTIONAL PART.

Data Kinds
----------

We've been talking about algebraic datatypes for months now, so these are nothing
new. But, with the right `LANGUAGE` extensions (I recommend `TypeInType`, but the
older `DataKinds` also works), you can use an algebraic datatype in a *kind*. So,
if we have

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

then we can say

> data T :: Nat -> Type where
>   MkT :: String -> T n

With this defintion, the constructor `MkT` (when applied to a `String`) gives us
a `T n` for any `Nat` `n`. For example, we could have `MkT "hi" :: T Zero` or
`MkT "bye" :: T (Succ (Succ Zero))`. Here, `Zero` and `Succ` are being used in
*types*, not ordinary expressions. (They're to the *right* of the `::`.)
So far, this feature looks utterly useless, but it won't be, soon.

One point of complication arises here, though: Haskell has two separate namespaces:
one for constructors and one for types. This is why we can have types like

> data SameName where
>   SameName :: Bool -> SameName

Here, `SameName` is a type and its constructor. This is idiomatic in Haskell, if
confusing for newcomers to the language. Normally, constructors and types are
written in different places in your code, so the re-use of the name isn't
problematic. However, if we can use constructors in types (as we have with `Zero`
and `Succ`) this *is* problematic. Haskell's solution is to use `'` (that is,
an apostrophe) to disambiguate. So, if a name is used as both a constructor and
a type, use `'` in a type to choose the constructor. So, the kind of the type
`SameName` is `Type`, but the kind of the type `'SameName` is `Bool -> SameName`.
GHC prints out constructors in types with the tick-marks.

There is also some new syntax in the definition of `T`: instead of listing
some type variables after the type name `T`, this definition lists `T`'s
*kind*, which is `Nat -> Type`. (`Type` is the more modern way of spelling the
kind `*`. It is imported from `Data.Kind`. `*` gets awfully confusing when you
also have multiplication in types -- which we will have soon enough. In any case,
`Type` and `*` are treated identically.) That is, if the type `T` is given a
`Nat`, it will be a `Type`. This syntax can be used for other constructions.
For example:

> data Tree :: Type -> Type where
>   Leaf :: Tree a
>   Node :: String -> a -> Tree a -> Tree a -> Tree a

Note that the kind of `Tree` is `Type -> Type`. This is the same as it always
was, but now the kind is written explicitly.

Length-indexed vectors
----------------------

One of the most common examples of a GADT is a length-indexed vector, which we'll
call `Vec`. It is a common example because we can explore all the interesting
aspects of GADTs with them, but they're simpler than many other examples. They
also have a practical use, but it may be some time before we can get there.

(Soon, we'll see how GADTs can be used to make checking the invariants of
[red-black trees](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree) happen
at compile-time. That's surely practical.)

Here is the definition of length-indexed vectors:

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

Before getting all into the types, let's look at what this means at runtime.
A `Vec` is just a list. Compare its definition with that of the ordinary
list type:

```haskell
data [] :: Type -> Type where
  []  :: [] a
  (:) :: a -> [] a -> [] a
infixr 5 :
```

The only difference between these definitions is `Vec`'s `Nat` index.
(The parameter to a type is sometimes called an index -- especially when
that parameter's kind is not `Type`.) Accordingly, you can use `Vec`s wherever
you can use a list.

That `Nat` index tracks the length of a `Vec`. We can see that the index
of `Nil` is always `Zero`. (We see that because the type of `Nil` is always
`Vec Zero a`. You can never have another number there.) We also see that
the index of the result of a cons (that is, a `:>`) is always one more than
the index of the tail of the list. (Here, I'm looking at the `Succ` in the
result type of `(:>)`.)

Let's see some examples. But before we can *see* them, we'll need a `Show`
instance. It would be nice if we could write `deriving Show` in the `Vec`
definition, but normal `deriving` doesn't work with GADTs. (Try it and see
what happens!) So we use another feature called "standalone-deriving" instead:

> deriving instance Show a => Show (Vec n a)

In a standalone-deriving declaration, you write `deriving` away from any
other definition and you give the entire instance header, including any
necessary context. You must also specify the `StandaloneDeriving` language
extension. (If you forget either the context or the extension, GHC helpfully
reminds you. Try this out!)

Now, we can define an example `Vec`:

> stuff = 5 :> 3 :> 8 :> Nil

First off, GHCi can happily print out `stuff`, showing us
`5 :> (3 :> (8 :> Nil))`. Those parentheses can be omitted, but the `Show`
instance isn't quite smart enough. What is `stuff`s type? (Think before you
look.) GHCi reports that it's `Vec ('Succ ('Succ ('Succ 'Zero))) Integer`.
Note the tick-marks in the printout. This type says that the length of the
`Vec` is 3. This should not be terribly surprising.

How can we use this? Let's walk through several examples.

First, we can define a `head` function that is guaranteed to be safe:

> safeHead :: Vec (Succ n) a -> a
> safeHead (x :> _) = x

Despite having only one equation, this function is total. GHC can see that
the index on the type of the argument is `(Succ n)`; therefore, the argument
cannot be `Nil`, whose index is `Zero`. Trying to add an equation
`safeHead Zero = error "urk"` is actually an error with `Inaccessible code`.
(Try it!) Being able to define `safeHead` is already a nice advantage of
use `Vec` over lists.

Naturally, we can have the counterpart to `safeHead`, `safeTail`. But the
type here will be a bit more involved, requiring us to think about the index
of the resulting `Vec`. If the input type's index is `Succ n`, well, the
output type's index had better be `n`:

> safeTail :: Vec (Succ n) a -> Vec n a
> safeTail (_ :> xs) = xs

Once again, this function is total even though it misses the `Nil` case.
Also of interest is that GHC checks to make sure that the return value really
is one element shorter than the input. See what happens if you try
`safeTail xs = xs`. GHC will notice that the index on the input `Vec` is not
`Succ` applied to the index on the output `Vec`.

Let's now write a recursive function, `snoc`. This function (`cons` spelled
backwards) appends to the *end* of a `Vec`. It takes an input `Vec`, a new
element, and produces an output `Vec`, one longer than the input:

> snoc :: Vec n a -> a -> Vec (Succ n) a
> snoc Nil       x = x :> Nil
> snoc (y :> ys) x = y :> snoc ys x

There's quite a bit of heavy lifting going on in the types here. In the
first equation, GHC learns that the index `n` is really `Zero`. So, the
return value must then have type `Vec (Succ Zero) a`. And, sure enough,
following the types of `Nil` and `:>` tells us that `x :> Nil` really
does have type `Vec (Succ Zero) a` (if `x :: a`).

In the second equation, we see that `y :> ys` has type `Vec n a`.
According to the type of `:>`, this means that `ys` must have type
`Vec m a` for some `m` and that `(y :> ys) :: Vec (Succ m) a`. But if
`(y :> ys) :: Vec n a` and `(y :> ys) :: Vec (Succ m) a`, this must
mean that `n` equals `Succ m`. (GHC writes `n ~ Succ m`. The `~` is
GHC's notation for type equality.) Since the return value must have
type `Vec (Succ n) a`, we now know that it must really have the type
`Vec (Succ (Succ m)) a`. Happily, the right-hand side of the equation
above, `y :> snoc ys x` really has that type. First, we see that
`snoc ys x` has type `Vec (Succ m) a` (recalling that `ys :: Vec m a`).
Then, `:>` just adds one more element.

Try playing with this definition to see that GHC will stop you from
making many mistakes. Of course, the types don't track the actual
contents of the `Vec`, so confusing `x` with `y` won't trigger a type
error.

We can now use `snoc` in another recursive function, `reverse`:

> reverse :: Vec n a -> Vec n a
> reverse Nil       = Nil
> reverse (x :> xs) = snoc (reverse xs) x

The type of `reverse` tells us that the output `Vec` has the same length
of the input `Vec`. This type also means that GHC checks to make sure the
implementation of `reverse` respects this property.

The definition is not all that remarkable, but it is worth taking the
time to trace through the types, in order to see why `reverse` type-checks.

Sadly, this implementation of `reverse` is quadratic in the length of the
list. (At every element, it calls `snoc`, which is a linear-time function.)
Doing better will require more type-level machinery, so we return to it
later.
