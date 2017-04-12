---
title: CS 380 Lab #7
---

<div id="header">

| **CS 380 Lab #7**
| Prof. Richard Eisenberg
| Spring 2017

</div>

Practical Haskell
-----------------

1. Write a Haskell program that loops, asking the user for a number and then reporting the number's prime factors. Exit
when the user enters in something other than a positive number.

2. Implement the following utilities in Haskell:

    a. `cat`
    b. `echo`
    c. `ls`, with its `-F` option
    d. `cp`

    The modules [`System.IO`](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-IO.html) and
    [`System.Directory`](http://hackage.haskell.org/package/directory-1.3.1.1/docs/System-Directory.html)
    will be helpful.

3. (Challenge) Ponder how to take your solution to (1) and add fancy types to it. Note that prime factorization has
a simple specification: it takes a number and produces a list of numbers, each of which is prime and the product of
which equals the original number. If you have your algorithm run over `SNat`s, you should be able to create a custom
list-like GADT for your output that captures these properties. You can then use such a structure to verify your
algorithm.
