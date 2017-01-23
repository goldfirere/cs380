---
title: CS 380 Style Guide
---

<div id="header">

| **CS 380 Style Guide**
| Prof. Richard Eisenberg
| Spring 2017

</div>

\$navbar\$

Code Formatting Standards and Guidelines
========================================

All organizations and companies have specific conventions for formatting code.
While these formatting rules may vary from place to place, they are essential
for making your code readable and for enabling others to understand, use, and
modify your code in the future.

[Here](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md) is
a longer style guide, but I prefer 2 spaces instead of the 4 advocated there.

Naming Conventions
------------------

 * Use meaningful names!  For example, if your program needs a variable to represent the radius of a circle, call it `radius`, *not* `r` and *not* `rad`.
 * Use single-letter variable names only for variables whose meaning is irrelevant. In other words, the
 type of a single-letter variable should generally be a type variable.
 * When naming elements of a list (especially one with type `[a]`), it is conventional to use `x` and `xs`;
 other letters are OK, too.
 * The use of very obvious, common, meaningful abbreviations is permitted. For example, "number" can be abbreviated as "num" as in `num_students`.
 * Haskell requires that types and constructors start with capital letters; variables start with lower-case letters.
 * Exported names should generally be written in `camelCase`.
 * Local names should generally be written `lower_case_with_underscores`. (This deviates from common
 Haskell practice, where all variables are in camel-case. But it's so much easier to read.)

Whitespace
----------

The most-readable programs are written with prudent use of whitespace
(including both blank lines and spaces).

 * Use blank lines to separate major parts of a source file or function. These are like paragraph breaks in English writing.
 * Functions should be separated by blank lines.
 * Haskell requires indentation in certain places. Indentation should generally be by 2 spaces.
 * Never ever ever ever ever ever ever use tabs.
 * Haskell code tends to be vertically aligned. For example, note how the `=` signs are aligned below:

```haskell
length :: [a] -> Int
length _      = 0
length (_:xs) = 1 + length xs
```

File header comments
--------------------

Every source code file should contain a header comment that describes the
contents of the file and other pertinent information. It must include the
following information:

  * The assignment number
  * Your name
  * Your school e-mail address
  * A description of the contents of the file

For example:

```haskell
{------------------------------------------
 | Assignment 4
 | Name:    Barbara Smith
 | E-mail:  bsmith22@brynmawr.edu
 |
 | The main driver program for project 4.
 |
 | This program reads the file specified as the first command line
 | argument, counts the number of words, spaces, and characters and
 | displays the results in the format specified in the project description.
 |
 -------------------------------------------}
```

Function comments
-----------------

*All* functions must be commented. Include descriptions of the parameters
(perhaps by commenting the parameter types) and what the function does. For example:

```haskell
-- Inserts the xs between every element of xss and concatenates the result.
intercalate :: [a]   -- xs
            -> [[a]] -- xss
            -> [a]
```

In-Line Comments
----------------

You should strive for your code to be self-explanatory. However, it is inevitable
that some lines of code are more intricate. In these cases, a comment describing
the code is well-advised. The comment should *not* simply translate the code to
English, but should explain what's really going on.

