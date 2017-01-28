---
title: "CS 380: Modern Functional Programming"
---

<div id="header">

| **CS 380: Modern Functional Programming**
| Prof. Richard Eisenberg
| Spring 2016
| Bryn Mawr College

</div>

\$navbar\$

General information
===================

<div id="info_table">

----------------------         -----------------------------------------------------------------------------------------------------------------------------------------
Instructor:                    [Richard Eisenberg](http://cs.brynmawr.edu/~rae)
Email:                         `rae@cs.brynmawr.edu`
Office Phone:                  610-526-5061
Home Phone (emergencies only): 484-344-5924
Cell Phone (emergencies only): 201-575-6474 (no texts, please)
Office:                        Park 249
Office Hours:                  Mondays 1:15-2:30, Tuesdays 2:30-3:30.
                               If these don't work, do not fret. Email instead.
<span class="strut" />
Lecture:                       MW 9:40-11:00
Lecture Room:                  Park 336
Lecture Recordings:            at [Tegrity](https://brynmawr.tegrity.com/#/course/b3842132-b03d-41bd-8064-3cf741138497)
Lab:                           W 11:10-12:30
Lab Room:                      Park 231
Website:                       <http://cs.brynmawr.edu/cs380>
GitHub Repo:                   <https://github.com/bmc-cs380/cs380>
Mailing List:                  [cs380-sp17@lists.cs.brynmawr.edu][mailing-list]
----------------------         -----------------------------------------------------------------------------------------------------------------------------------------

</div>

Goals of course
---------------

<div id="goals">

By the end of this course, you will be able to...

* write programs in a pure, typed, functional language: Haskell
* use dependent types to encode static guarantees in your programs
* explain the difference between (functional vs. imperative), (statically typed vs. dynamically checked), (pure vs. effectful)
* contribute to the debate between static types vs. dynamic checks
* apply your knowledge of Haskell to imperative settings

During the course, you will...

* code in Haskell
* occasionally write about comparisons between language designs
* work with other students every class

</div>

This is a course in typed, functional programming using Haskell. Haskell
was invented by committee in 1990 to experiment with the possibilities
of lazy, typed, pure functional langauge. It has since grown substantially
and now attracts a small but growing industrial user base. In particular,
Haskell's type system is often recognized as best-in-class; novel ideas about
type systems are frequently phrased in terms of Haskell.

During this course, you will learn Haskell with an eye to understanding the
benefits, drawbacks, and power of its type system. While Haskell can indeed
be used for quotidian tasks (writing games, interacting through websites
through their APIs, processing data), we will instead focus on types. By the
end of the course, you will learn about *dependent types* which give you
the ability to encode machine-checked invariants in your programs. For example,
you can give a `sort` function a type that says that the output list must
appear in non-decreasing order. If the implementation of such a function is
incorrect, the program is rejected at compile time.

Course Philosophy
-----------------

In order to learn any programming skill, you must simply *do* it. Reading a website
or watching me code will not help you get better. This course is thus
*programming-intensive*. You will be expected to spend a significant amount of
time weekly (6-8 hours) outside of class programming to complete the homework
assignments. If you run into a snag, the programming burden may prove to be
even higher. Of course, programming is fun, so the time should fly by.

Class time in CS380 will be spent primarily working with peers to solve small
programming problems, conduct peer reviews, and complete other exercises. It
is expected that you complete the reading before class and come with questions;
I will try to avoid spending large chunks of class time repeating what the reading
has already covered.

Materials
=========

There is no required textbook for this course. Below are several resources you might
find helpful while learning Haskell; however, none of them dive into the type
system the way we will in this course.

* **Haskell from First Principles**, by Christopher Allen and Julie Moronuki. This comprehensive
book has been the multi-year effort of two prominent Haskell educators, who have refined the
presentation of Haskell in this book as they have taught many classes of students. This was nearly
the selected textbook for this course, but it does not have enough on Haskell's advanced type system
features for this course. I recommend it as a companion text, however.
The book is currently
online-only (for \$\$59), available at <http://haskellbook.com/>.

* **Real World Haskell**, by Bryan O'Sullivan, Don Stewart, and John Goerzen. This book
has been a standard text for learning Haskell for some time, but it is now somewhat out
of date. I learned Haskell from this book. It is available for free online at
<http://book.realworldhaskell.org/>.

* **Learn You a Haskell for Great Good**, by Miran Lipova&#269;a. This is another, slightly
outdated book on learning Haskell. It is also free online at <http://learnyouahaskell.com/>.

* Stephen Diehl has written a comprehensive page covering all kinds of Haskell goodies at
<http://dev.stephendiehl.com/hask/>.

* Lots of tutorials are available at <https://www.schoolofhaskell.com/>.

It can be hard to sort through all of this. If you find a tidbit (individual chapter of a book,
tutorial, etc.) that you find helpful, [tell us all][mailing-list].

[mailing-list]: mailto:cs380-sp17@lists.cs.brynmawr.edu
