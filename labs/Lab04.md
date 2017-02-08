---
title: CS 380 Lab #4
---

<div id="header">

| **CS 380 Lab #4**
| Prof. Richard Eisenberg
| Spring 2017

</div>

1. Peer review

    Taking turns, each member of your group should seek peer review about their
    solutions to hw02. Although scrutiny of any of hw02 is a good idea, make
    sure to discuss at least these functions:
    
      - `findTree`
      - `frequencies`

    If you end up resubmitting your assignment after class, please cite your
    partners as resources if their input has helped.

    Compare different styles and approaches to the problems. How did the others
    figure out the answers? Do any of you have lingering questions?

    During this process, remember that everyone in this class has a different
    level of experience and be supportive of one anothers' challenges. This is
    not a time to show off!

    I expect this will take at least 20 minutes to get through everyone's work.

2. Download the [Peano.hs](../07/Peano.hs) file from today's class. Write the following
functions in the file, and test them with HUnit:

        minus :: Nat -> Nat -> Nat
        equal :: Nat -> Nat -> Bool
        lessThanEqual :: Nat -> Nat -> Bool
        evenNat :: Nat -> Bool

3. Prove the following theorems:

    a. `plus` is associative
    b. `mult n Zero` is `Zero`
    c. `mult` is commutative

4. It is *not* the case that `minus (plus a b) c` equals `plus a (minus b c)`. Why? Try
to prove this non-fact; where does the proof go wrong?
