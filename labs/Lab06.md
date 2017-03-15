---
title: CS 380 Lab #6
---

<div id="header">

| **CS 380 Lab #6**
| Prof. Richard Eisenberg
| Spring 2017

</div>

Functions on `Vec`s
-------------------

Consulting the documentation on [lists](https://www.stackage.org/haddock/lts-8.5/base-4.9.1.0/GHC-OldList.html), come up with types and implementations for the
translation of the following functions to work on `Vec`s. There should be
no lists in your code.

These are put in order of expected difficulty.

1. `and`
2. `or`
3. `any`
4. `map`
5. `last`
6. `unzip`
7. `uncons` (no need to use `Maybe`)
8. `init`
9. `insert`
10. `sort` (use insertion sort)
11. `null` (this can return an `SBool`)
12. `length` (this can return an `SNat`)
13. `stripPrefix`
14. `take` (this takes an `SNat`)
15. `drop` (this takes an `SNat`)
16. `replicate` (this *takes* an `SNat`)

Here are some starter definitions you might find helpful:

```haskell
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

type family (a :: Nat) + (b :: Nat) :: Nat where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- singleton Bool
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
```
