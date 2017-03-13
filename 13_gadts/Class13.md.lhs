---
title: CS 380 Class 13 Lecture Notes
---

Class 13 -- GADTs
=================

This file is a Literate Haskell file. If you download it in LHS format from
the website, you will be able to compile it in GHCi. When a file ends in the
extension .lhs, GHC looks for lines beginning with > and compiles only those.
Everything else is treated as a comment.

(This file is also in Markdown format for easy rendering to HTML, using
Pandoc.)

As we've seen a few times, Haskell supports *extensions*, enabling you to turn
on certain language features if you want them. From here on out, we'll be
using a bunch.

> {-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}

> module Class13 where

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

At first, this doesn't seem like much of a change. It just looks like a
slightly more restrictive type for the constructors. So, `MkGInt 5` has type
`G Int` and `MkGBool` has type `G Bool`. So far, so good. But things get more
interesting when we pattern match:

> foo :: G a -> a
> foo (MkGInt n)  = n + 5
> foo (MkGBool b) = not b

Whoa! The different equations *return different types*. And we're using
non-parametric operations `(+)` and `not`, even though the type is given as `G
a -> a`, where we haven't specified the type `a`. (Just try *that* with
`Maybe`!)

What's going on here is that the pattern-match tells us two things: it tells
us what constructor was used to build the `G a`, but it also tells us what `a`
is. When we know that the constructor is `MkGInt`, we know `a` is `Int`. (That
is, `a ~ Int` in Haskell notation.) When we know that the constructor is
`MkGBool`, we know that `a` is `Bool`. So, in the right-hand sides of these
equations, we can use our knowledge of `a`, returning an `Int` in the first
equation and a `Bool` in the second. We can even do this:

> bar :: G a -> a -> Bool
> bar (MkGInt _)  x = x > 0
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
