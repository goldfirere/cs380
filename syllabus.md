---
title: CS 380 Syllabus
---

<div id="header">

| **CS 380 Syllabus**
| Prof. Richard Eisenberg
| Spring 2017

</div>

\$navbar\$

All information in the future is subject to change, though this calendar
represents my aims.

+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|\# |Date |Topic               | Reading        |Examples          |  Assignments       |   Labs             |  Extras           |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|1  |1/18 | * Introductions    | Chapters 1-3, 5| [Intro.hs] from  |                    |  [Lab 1]\: Basic   | [Types in Math]   |
|   |     | * Functional       |at              |class             |                    |Haskell             |                   |
|   |     |programming         |[learn.hfm.io]  |                  |                    |                    |                   |
|   |     | * Types            |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 2 |1/23 | * Higher-order     | * The [Haskell |                  | [Assignment 1] out |                    | [Higher-order     |
|   |     |functions           |Report], Ch. 1, |                  |                    |                    |functions]         |
|   |     | * Parametric       |Sec. 2.7        |                  | [Course survey]    |                    |                   |
|   |     |polymorphism        | * [Sections]   |                  |                    |                    |                   |
|   |     | * Lists            |                |                  |                    |                    |                   |
|   |     | * `map`            |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 3 |1/25 | * Types            | no new reading | [Testing.hs]     |                    |  [Lab 2]\: Lists   |                   |
|   |     | * Unit testing     |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 4 |1/30 | * Lists            | * [Chapter 3   | [Lists.hs]       | [Assignment 1] due |                    | [List             |
|   |     | * Modules          |from Real World |                  |                    |                    |comprehension      |
|   |     |                    |Haskell]        |                  |                    |                    |exercises]         |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 5 |2/1  | * Algebraic data   |                |                  |                    | [Lab 3]\: Trees    |                   |
|   |     |types               |                | [Class.hs][05c]  | [Assignment 2] out |                    |                   |
|   |     | * Maybe            |                |from class        |                    |                    |                   |
|   |     | * Trees            |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 6 |2/6  | * More on Trees    |                | *                |                    |                    | [Maybe.hs] from   |
|   |     |                    |                |[Arith.hs][ar1]   |                    |                    |class              |
|   |     |                    |                | *                |                    |                    |                   |
|   |     |                    |                |[Parser.hs][pa1]  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 7 |2/8  | * Parametricity    | * [Chapter 6   | [Peano.hs]       |[Assignment 2] due  | [Lab 4]\: Peano    | [Parametricity    |
|   |     | * Proofs over      |from Real World |                  |                    |                    |exercises]         |
|   |     |functional programs |Haskell], up to |                  |Assignment 3,       |                    |                   |
|   |     |                    |(but not        |                  |[Halgebra.hs], out  |                    |                   |
|   |     |                    |including) the  |                  |                    |                    |                   |
|   |     |                    |JSON part       |                  | * [Arith.hs][ar2]  |                    |                   |
|   |     |                    |                |                  | * [Parser.hs][pa2] |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|   |2/13 |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
| 8 |2/15 | * More poofs       |                | [Class.hs][c8]   | [Assignment 3] due |                    | * [I/O functions] |
|   |     | * I/O              |                |I/O example       |                    |                    | * [HTTP           |
|   |     |                    |                |                  |                    |                    |functions]         |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|9  |2/20 | * Type classes     |                | [Class.hs][c9]   | [Assignment 4] out |                    | [markets.json]    |
|   |     |(`Num`, `Eq`,       |                |classes example   |                    |                    |                   |
|   |     |`Monoid`, etc.)     |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|10 |2/22 | * Functors         |  * [Intro to   | [Class.hs][c10]  |                    |                    |                   |
|   |     | * Monads           |QuickCheck][qc1]|examples from     |                    | [Lab 5]\: Proofs   |                   |
|   |     |                    |  * [Further    |class             |                    |                    |                   |
|   |     |                    |reading][qc2]   |                  |                    |                    |                   |
|   |     |                    |                |[FoldMap.hs]      |                    |                    |                   |
|   |     |                    |I use QuickCheck|                  |                    |                    |                   |
|   |     |                    |to evaluate your|                  |                    |                    |                   |
|   |     |                    |homework. You   |                  |                    |                    |                   |
|   |     |                    |should, too.    |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|11 |2/27 | * Review           |                |                  | [Assignment 4] due |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|12 |3/1  |  **Exam 1**        |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|   |3/6  |**SPRING BREAK**    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|   |3/8  |**SPRING BREAK**    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|13 |3/13 | * GADTs            | [Red-black     | [Class.hs][c13]  |                    |                    |                   |
|   |     | * `TypeRep`        |trees], at least|   (this includes |                    |                    | * GADT lecture    |
|   |     | * `Vec`            |the intro and   |class 14)         |                    |                    |notes [html][ht13] |
|   |     |                    |"Properties".   |                  |                    |                    |[lhs][lhs13]       |
|   |     |                    |Read enough so  |                  |                    |                    |                   |
|   |     |                    |that you        |                  |                    |                    |                   |
|   |     |                    |understand this |                  |                    |                    |                   |
|   |     |                    |data structure. |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|14 |3/15 | * More `Vec`       |                |                  | [Assignment 5] out | [Lab 6]\: `Vec`    |                   |
|   |     | * Type families    |                |                  | (part of portfolio)|                    | * Type families   |
|   |     | * Singletons       |                |                  |                    |                    |lecture notes      |
|   |     |                    |                |                  |                    |                    |[html][ht14]       |
|   |     |                    |                |                  |                    |                    |[lhs][lhs14]       |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|15 |3/20 | * Proofs in Haskell|                | [Class.hs][c15]  |                    |                    | * Proofs lecture  |
|   |     |                    |                |                  |                    |                    |notes [html][ht15] |
|   |     |                    |                |                  |                    |                    |[lhs][lhs15]       |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|   |3/22 |**Community day of  |                |                  | [Assignment 6] out |                    |                   |
|   |     |learning**          |                |                  |(part of portfolio) |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |(no class)          |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|16 |3/27 | * `VecList`        |                | [Class.hs][c16]  |                    |                    | * `VecList`       |
|   |     |                    |                |                  |                    |                    |lecture notes      |
|   |     |                    |                |                  |                    |                    |[html][ht152]      |
|   |     |                    |                |                  |                    |                    |[lhs][lhs152]      |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|17 |3/29 | * `Fin`            |                | [Class.hs][c17]  | [Assignment 7] out |                    | * `Fin` lecture   |
|   |     | * Existentials     |                |                  |(part of portfolio) |                    |notes [html][htfin]|
|   |     | * Implicits        |                |                  |                    |                    |[lhs][lhsfin]      |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|18 |4/3  | * Existential      |                | *                |                    |                    | * Existentials    |
|   |     |properties          |                |[Class.hs][c18]   |                    |                    |lecture notes      |
|   |     | * `Maybe` monad    |                | *                |                    |                    |[html][htex]       |
|   |     |                    |                |[MaybeMonad.hs]   |                    |                    |[lhs][lhsex]       |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|19 |4/5  | * `OrdList`        |                | *                | [Assignment 8] out |                    |                   |
|   |     |                    |                |[ClassOrdList.hs] |(part of portfolio) |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|20 |4/10 | Guest Lecture:     |                |                  |                    |                    |                   |
|   |     |Kenny Foner on using|                |                  |                    |                    |                   |
|   |     |an SMT solver for   |                |                  |                    |                    |                   |
|   |     |type checking       |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|21 |4/12 | * `OrdList`        |                | * [OrdList.hs]   | Function portfolio | [Lab 7]\: Practical|                   |
|   |     | * Practical Haskell|                | * [Practical.hs] |due                 |Haskell             |                   |
|   |     |                    |                |                  |                    |                    |                   |
|   |     |                    |                |                  | [Final project] out|                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|22 |4/17 | * Monads           |                | * [NatVec.hs]    |                    |                    |                   |
|   |     |                    |                | *                |                    |                    |                   |
|   |     |                    |                |[Arith.hs][ar22]  |                    |                    |                   |
|   |     |                    |                | *                |                    |                    |                   |
|   |     |                    |                |[Parser.hs][pa22] |                    |                    |                   |
|   |     |                    |                | * [Eval.hs][ev22]|                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|23 |4/19 | * Review           |                | * [Pong.hs]      | Final project      |                    | * [Practice Exam  |
|   |     |                    |                |                  |proposal due        |                    |2]                 |
|   |     |                    |                |                  |                    |                    | * [Exam2.hs]      |
|   |     |                    |                |                  |                    |                    | * [Exam2-sol.hs]  |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|24 |4/24 |                    |                |                  |                    |                    |                   |
|   |     | **Exam 2**         |                |                  |                    |                    |                   |
|   |     |                    |                |                  |                    |                    | * [Exam2.hs][e24] |
|   |     |                    |                |                  |                    |                    |                   |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+
|25 |4/26 | * Functional       |                |                  |                    |                    |                   |
|   |     |programming in Java |                |                  |                    |                    | * Java lecture    |
|   |     |                    |                |                  |                    |                    |notes [html][htj]  |
|   |     |                    |                |                  |                    |                    |[lhs][lhsj]        |
|   |     |                    |                |                  |                    |                    |[java][jj]         |
+---+-----+--------------------+----------------+------------------+--------------------+--------------------+-------------------+

**Monday, May 1, 2:00-5:00, Park 229:** Project presentations.

[learn.hfm.io]: http://learn.hfm.io/
[Types in math]: 01/types.pdf
[Lab 1]: labs/Lab01.hs
[Haskell Report]: https://www.haskell.org/onlinereport/haskell2010/
[Sections]: https://wiki.haskell.org/Section_of_an_infix_operator
[Assignment 1]: hw01/Intro.hs
[Higher-order functions]: 02/exercises.pdf
[Intro.hs]: 01/Intro.hs
[Course survey]: https://docs.google.com/forms/d/e/1FAIpQLScwTPjvehHXtIR0j14ygq_72_ZULOFhajYMp_d79621bT1lRA/viewform
[Testing.hs]: 03/Testing.hs
[Lab 2]: labs/Lab02.hs
[Chapter 3 from Real World Haskell]: http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
[List comprehension exercises]: 04/exercises.pdf
[Lists.hs]: 04/Lists.hs
[Lab 3]: labs/Lab03.hs
[Assignment 2]: hw02/Hw02.hs
[05c]: 05/Class.hs
[ar1]: 06/Arith.hs
[pa1]: 06/Parser.hs
[Maybe.hs]: 06/Maybe.hs
[Chapter 6 from Real World Haskell]: http://book.realworldhaskell.org/read/using-typeclasses.html
[Lab 4]: labs/Lab04.html
[Peano.hs]: 07/Peano.hs
[Parametricity exercises]: 07/parametricity.pdf
[Halgebra.hs]: hw03/Halgebra.hs
[Assignment 3]: hw03/Halgebra.hs
[ar2]: hw03/Arith.hs
[pa2]: hw03/Parser.hs
[c8]: 08/Class.hs
[I/O functions]: http://hackage.haskell.org/package/base-4.9.1.0/docs/System-IO.html
[HTTP functions]: http://hackage.haskell.org/package/HTTP-4000.3.5/docs/Network-HTTP.html
[HTTP example]: 08/Lab.hs
[Assignment 4]: 09/markets.pdf
[markets.json]: 09/markets.json
[c9]: 09/Class.hs
[qc1]: https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing
[qc2]: https://www.stuartgunter.org/posts/intro-to-quickcheck/
[Lab 5]: labs/Lab05.html
[c10]: 10/Class.hs
[FoldMap.hs]: 10/FoldMap.hs
[Red-black trees]: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree
[ht13]: 13/GADTs.html
[lhs13]: 13/GADTs.md.lhs
[c13]: 13/Class.hs
[Lab 6]: labs/Lab06.html
[Assignment 5]: hw05/hw05.html
[ht14]: 14/TypeFamilies.html
[lhs14]: 14/TypeFamilies.md.lhs
[ht15]: 15/Proofs.html
[lhs15]: 15/Proofs.md.lhs
[c15]: 15/Class.hs
[ht152]: 15/VecList.html
[lhs152]: 15/VecList.md.lhs
[Assignment 6]: hw06/hw06.html
[c16]: 16/Class.hs
[Assignment 7]: hw07/hw07.html
[c17]: 17/Class.hs
[htfin]: 17/Fin.html
[lhsfin]: 17/Fin.md.lhs
[htex]: 17/Existentials.html
[lhsex]: 17/Existentials.md.lhs
[c18]: 18/Class.hs
[MaybeMonad.hs]: 18/MaybeMonad.hs
[ClassOrdList.hs]: 19/ClassOrdList.hs
[Assignment 8]: hw08/hw08.html
[Final project]: hw09/final.html
[Lab 7]: labs/Lab07.html
[Practical.hs]: 21/Practical.hs
[OrdList.hs]: 21/ClassOrdList.hs
[NatVec.hs]: 22/NatVec.hs
[ar22]: 22/Arith.hs
[pa22]: 22/Parser.hs
[ev22]: 22/Eval.hs
[Pong.hs]: 22/Pong.hs
[Practice Exam 2]: 23/exam2-practice.pdf
[Exam2.hs]: 23/Exam2.hs
[Exam2-sol.hs]: 23/Exam2-sol.hs
[e24]: 24/Exam2.hs
[htj]: 25/Java.html
[lhsj]: 25/Java.md.lhs
[jj]: 25/Haskell.java
