---
title: CS 380 Lab #5
---

<div id="header">

| **CS 380 Lab #5**
| Prof. Richard Eisenberg
| Spring 2017

</div>

1. Peer review

    Taking turns, each member of your group should seek peer review about their
    solutions to hw03. Although scrutiny of any of hw03 is a good idea, make
    sure to discuss at least the `simpleSum`/`simpleTerm`/`simpleFactor` functions.

    Compare different styles and approaches to the problems. How did the others
    figure out the answers? Do any of you have lingering questions?

    During this process, remember that everyone in this class has a different
    level of experience and be supportive of one anothers' challenges. This is
    not a time to show off!

    I expect this will take at least 20 minutes to get through everyone's work.

2. Prove that `Succ Zero` is both a left-identity (e.g., `mult (Succ Zero) n = n`) and a
   right-identity (e.g., `mult n (Succ Zero) = n`) of `mult`. You may assume a lemma that
   says that, for all `n`, `plus n Zero = n`. Work with your partner(s) on this, and show
   me the final result. This is your chance to get feedback on a proof before next week's
   exam. (You can structure your proof as two separate proofs; one for left-identity and
   one for right-identity.)

3. Write a `Monoid` instance for your `Tree` datatype from hw02. Challenge problem:
Prove the monoid laws of your instance:
    a. `m <> empty == m`
    b. `empty <> m == m`
    c. `(m1 <> m2) <> m3 == m1 <> (m2 <> m3)`

4. Begin hw04. Ask questions. The code for hw04 is not very intricate, but the setup is
   rather involved. Do take a close look at this before Sunday!



