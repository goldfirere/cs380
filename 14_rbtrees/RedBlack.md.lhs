---
title: Red-Black Trees
---

Red-Black Trees
===============

(Credit to Stephanie Weirich, who originally developed this code.)

This module implements a common balanced tree structure:
[Red-black trees](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree).

> {-# LANGUAGE GADTSyntax, KindSignatures, TypeApplications,
>              ScopedTypeVariables #-}
> module RedBlack where
> import Data.Kind        ( Type )
> import Data.Maybe       ( isJust )
> import Control.Monad    ( guard )
> import Test.QuickCheck  ( Arbitrary(..), quickCheck )

A red-black tree is a binary search tree where every node is
marked with a color (red or black).

> data Color where
>   Red :: Color
>   Blk :: Color
>   deriving (Eq, Show)

> data RBT :: Type -> Type where
>   Lf :: RBT a
>   Nd :: Color -> RBT a -> a -> RBT a -> RBT a
>   deriving (Eq, Show)

> -- retrieves the color of a tree
> color :: RBT a -> Color
> color (Nd c _ _ _) = c
> color Lf           = Blk

Furthermore, red-black trees must satisfy the following
invariants.

  1. The leaves are black

  2. The root is always black

  3. From each node, every path to a leaf
     has the same number of black nodes

  4. Red nodes have black children

The first invariant is true by definition, the others we will have to maintain
as we implement the tree. Together, these invariants imply that every
red-black tree is "approximately balanced", in the sense that the longest path
to an empty node is no more than twice the length of the shortest.
From this, it follows that all operations will run in $O(\log_2\ n)$
time.

Sample Trees
------------

Here is a good tree, satisfying the invariants:

> good1 :: RBT Int
> good1 = Nd Blk (Nd Blk Lf 1 Lf) 2 (Nd Blk Lf 3 Lf)

But here the root is red:

> bad1 :: RBT Int
> bad1  = Nd Red (Nd Blk Lf 1 Lf) 2 (Nd Blk Lf 3 Lf)

Here the black heights are different:

> bad2 :: RBT Int
> bad2  = Nd Blk (Nd Red Lf 1 Lf) 2 (Nd Blk Lf 3 Lf)

This one has a red child of a red node:

> bad3  :: RBT Int
> bad3  = Nd Blk (Nd Red (Nd Red Lf 1 Lf) 2 (Nd Red Lf 3 Lf)) 4 Lf

All sample trees

> trees :: [RBT Int]
> trees = [good1, bad1, bad2, bad3]

Checking the RBT invariants
---------------------------

We can write functions to check each of the invariants.

1. The empty nodes at the leaves are black.

> prop_Rb1 :: Bool
> prop_Rb1 = color Lf == Blk

2. The root of the tree is Black.

> prop_Rb2 :: RBT Int -> Bool
> prop_Rb2 t = color t == Blk

3.  For all nodes in the tree, all downward paths from the
node to a leaf contain the same number of Black nodes.

> prop_Rb3 :: RBT Int -> Bool
> prop_Rb3 t = isJust (go t)
>   where
>     go :: RBT Int -> Maybe Int
>     go Lf = Just 0
>     go (Nd c l _ r) = do
>       depth_l <- go l
>       depth_r <- go r
>       guard (depth_l == depth_r)
>       case c of
>         Red -> return depth_l
>         Blk -> return (1 + depth_l)

4. All children of red nodes are black.

> prop_Rb4 :: RBT Int -> Bool
> prop_Rb4 Lf = True
> prop_Rb4 (Nd c l _ r)
>   = prop_Rb4 l && prop_Rb4 r &&
>     case c of
>       Red -> color l == Blk && color r == Blk
>       Blk -> True

To keep ourselves honest, we'll use QuickCheck, a library that can generate
trees randomly for testing. QuickCheck does this via its `Arbitrary` class.
The details here are not important, but perhaps you will be inspired to learn
more about QuickCheck.

> instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
>    arbitrary = do
>      as <- arbitrary @[a]            -- generate an arbitrary list of `a`s
>      return $ foldr insert empty as  -- build a tree by inserting them all

The `checkProps` action runs our tests, confirming that the functions below
work as designed.

> checkProps :: IO ()
> checkProps = do
>   putStrLn "Leaves are black"
>   quickCheck prop_Rb1
>   putStrLn "Root is black"
>   quickCheck prop_Rb2
>   putStrLn "Black height the same"
>   quickCheck prop_Rb3
>   putStrLn "Red nodes have black children"
>   quickCheck prop_Rb4

Implementation
--------------

We consider our red-black trees to be *sets*, that is, unordered collections
of elements. These red-black trees are also binary search trees.
A set supports insertion, iteration, and a membership check.

> -- an empty red-black tree
> empty :: RBT a
> empty = Lf

> -- test whether an element is a member of the set
> member :: Ord a => a -> RBT a -> Bool
> member _ Lf = False
> member x (Nd _ a y b)
>   | x < y     = member x a
>   | x > y     = member x b
>   | otherwise = True

> -- produce a list of elements in the set
> elements :: Ord a => RBT a -> [a]
> elements Lf           = []
> elements (Nd _ l v r) = elements l ++ [v] ++ elements r

Insertion, is, of course a bit trickier.

> -- add a new element to the set
> insert :: Ord a => a -> RBT a -> RBT a
> insert x t = blacken (ins x t)

This is defined with the help of a recursive function `ins`.
That function walks down the tree until
it gets to an empty leaf node, in which case
it constructs a new (red) node containing the
value being inserted...

> ins :: Ord a => a -> RBT a -> RBT a
> ins x Lf = Nd Red Lf x Lf

... or discovers that the value being inserted is
already in the tree, in which case it returns
the input unchanged:

> ins x s@(Nd c a y b)
>   | x < y     = balanceL (Nd c (ins x a) y b)
>   | x > y     = balanceR (Nd c a y (ins x b))
>   | otherwise = s

Blackening
----------

Note that `ins` creates a tree with a red root when we insert into an empty
tree.  Our first fix to insert is to blacken the top node of the tree to make
sure that invariant (2) is always satisfied.

> blacken :: RBT a -> RBT a
> blacken Lf           = Lf
> blacken (Nd _ l v r) = Nd Blk l v r

Balancing
---------

In the recursive calls of `ins`, before returning the new tree, however, we
may need to *rebalance* to maintain the red-black invariants. The code to do
this is encapsulated in a helper function `balance`.

* The key insight in writing the balancing function is that we do not try to
rebalance as soon as we see a red node with a red child. That can be fixed
just by blackening the root of the tree, so we return this tree as-is.  (We
call such trees, which violate invariants two and four only at the root
"infrared").

The real problem comes when we've inserted a new red node between a black
parent and a red child. In other words, the job of the balance function is to
rebalance trees with a black-red-red path starting at the root.
The result of rebalancing maintains the black height by converting
to a red parent with black children.
Since the root has two children and four grandchildren, there are
four ways in which such a path can happen.

> balanceL :: RBT a -> RBT a
> balanceL (Nd Blk (Nd Red (Nd Red a x b) y c) z d)
>   =  Nd Red (Nd Blk a x b) y (Nd Blk c z d)
> balanceL (Nd Blk (Nd Red a x (Nd Red b y c)) z d)
>   =  Nd Red (Nd Blk a x b) y (Nd Blk c z d)
> balanceL t = t

> balanceR :: RBT a -> RBT a
> balanceR (Nd Blk a x (Nd Red (Nd Red b y c) z d))
>   =  Nd Red (Nd Blk a x b) y (Nd Blk c z d)
> balanceR (Nd Blk a x (Nd Red b y (Nd Red c z d)))
>   =  Nd Red (Nd Blk a x b) y (Nd Blk c z d)
> balanceR t = t

Red-Black deletion
------------------

We won't get to this in class, but here is an implementation of *deletion*
from Red/Black trees (taken from [1] below).

Deletion works by first finding the appropriate place in the tree to delete
the given element (if it exists).  At the node where we find the element, we
delete it by merging the two subtrees together.  At other nodes, when we call
delete recursively on one of the two subtrees, we may change the black height
of that subtree, so we will need to rebalance to restore the invariants.

This implementation maintains the invariant that deleting an element from a
*black* tree of height n + 1 returns a tree of height n, while deletion from
red trees (and the empty tree) preserves the height.  Even if the element is
not in the tree we can maintain this invariant by reddening the node (and
potentially producing an infrared tree.) As above, we blacken the final result
to restore this invariant.


> delete :: Ord a => a -> RBT a -> RBT a
> delete x t = blacken (del t)
>   where
>     del Lf = Lf
>     del (Nd _ a y b)
>         | x < y     = delLeft  a y b
>         | x > y     = delRight a y b
>         | otherwise = merge a b

Delete from the left subtree. If the left subtree is a black node, we need to
rebalance because its black height has changed.

>     delLeft a@(Nd Blk _ _ _) y b = balLeft (del a) y b
>     delLeft a                y b = Nd Red (del a) y b

Rebalancing function after a left deletion from a black-rooted tree. We know
that the black height of the left subtree is one less than the black height of
the right tree. We want to return a new, balanced (though potentially
infrared) tree.

>     balLeft :: RBT a -> a -> RBT a -> RBT a
>     balLeft (Nd Red a x b) y c               = Nd Red (Nd Blk a x b) y c
>     balLeft bl x (Nd Blk a y b)              = balanceR (Nd Blk bl x (Nd Red a y b))
>     balLeft bl x (Nd Red (Nd Blk a y b) z c) = Nd Red (Nd Blk bl x a) y (balanceR (Nd Blk b z (sub1 c)))
>     balLeft _ _ _ = error "invariant violation"

Helper function to reduce the black height of a tree by one by reddening the
node. Should only be called on black nodes. We know that `c` above is a black node because
* it is the child of a red node
* `c` must have the same black height as `(Nd Blk a y b)` so it can't be `Lf`

>     sub1 :: RBT a -> RBT a
>     sub1 (Nd Blk a x b) = Nd Red a x b
>     sub1 _ = error "invariant violation"

Deletion from the right subtree. Symmetric to the above code.

>     delRight a y b@(Nd Blk _ _ _) = balRight a y (del b)
>     delRight a y b             = Nd Red a y (del b)

>     balRight :: RBT a -> a -> RBT a -> RBT a
>     balRight a x (Nd Red b y c)               = Nd Red a x (Nd Blk b y c)
>     balRight (Nd Blk a x b) y bl              = balanceL (Nd Blk (Nd Red a x b) y bl)
>     balRight (Nd Red a x (Nd Blk b y c)) z bl = Nd Red (balanceL (Nd Blk (sub1 a) x b)) y (Nd Blk c z bl)
>     balRight _ _ _ = error "invariant violation"

Glue two red black trees together into a single tree (after deleting the
element in the middle). If one subtree is red and the other black, we can call
merge recursively, pushing the red node up. Otherwise, if both subtrees are
black or both red, we can merge the inner pair of subtrees together. If that
result is red, then we can promote it's value up. Otherwise, we may need to
rebalance.

>     merge :: RBT a -> RBT a -> RBT a
>     merge Lf x = x
>     merge x Lf = x
>     merge (Nd Red a x b) (Nd Red c y d) =
>       case merge b c of
>         Nd Red b' z c' -> Nd Red (Nd Red a x b') z (Nd Red c' y d)
>         bc -> Nd Red a x (Nd Red bc y d)
>     merge (Nd Blk a x b) (Nd Blk c y d) =
>       case merge b c of
>         Nd Red b' z c' -> Nd Red  (Nd Blk a x b') z (Nd Blk c' y d)
>         bc -> balLeft a x (Nd Blk bc y d)
>     merge a (Nd Red b x c)           = Nd Red (merge a b) x c
>     merge (Nd Red a x b) c           = Nd Red a x (merge b c)

Notes
-----

[0] See also persistant [Java
implementation](http://wiki.edinburghhacklab.com/PersistentRedBlackTreeSet)
for comparison. Requires ~350 lines for the same implementation.

[1] Stefan Kahrs, "Red-black trees with types", Journal of functional programming, 11(04), pp 425-432, July 2001

[2] Andrew Appel, ["Efficient Verified Red-Black Trees"](http://www.cs.princeton.edu/~appel/papers/redblack.pdf)
    September 2011. Presents a Coq implementation of
    a verified Red Black Tree based on Kahrs's implementation.

[3] Matt Might has a blog post on an alternative version of the [RBT deletion operation](http://matt.might.net/articles/red-black-delete/).
