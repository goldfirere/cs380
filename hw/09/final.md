---
title: CS 380 Final Project
---

<div id="header">

| **CS 380 Final Project**
| Prof. Richard Eisenberg
| Spring 2017
|
| Proposal due date: Wednesday, April 19, 2017
| Project/paper due date: Monday, May 1, 2017

</div>

Haskell Final Project
=====================

You've all come a long way since the beginning of the semester! Now is your chance to show
off what you've learned in a larger, cohesive project.

There are three components of the final project: the proposal (20 pts), the project itself
(65 pts), the presentation (5 pts), and the paper (10 pts). You may choose to work with up to one partner
on this project; if you do so, you and your partner will work together on the entire project, except the
paper.
Before getting into the details of these tasks,
let's explore the two possible project paths:

Path 1: Practical Haskell
-------------------------

This semester has dwelt only ever so briefly on the more practical aspects of Haskell, instead
exploring the fun of functional programming and Haskell's powerful type system. For those of
you itching for more practical knowledge of Haskell, here's your chance!

For this project, you can use Haskell to write a practical program that does something.
The something can be a small game, a utility, a little web site powered by Haskell, a
data visualizer or manipulator, etc. To do this, you will use some well-known Haskell
libraries, all available on Hackage. Here are some suggestions of packages:

* `aeson`, a JSON parser/printer
* `cassava`, a CSV (comma-separated value) parser/printer, useful for data manipulation tasks
* `yesod`, a web framework
* `parsec`, a general parser framework (allowing you to read a custom data format)
* `servant`, a web API framework
* `diagrams`, an API for making composable visual diagrams
* `gloss`, a graphics engine (with good support for making simple games)

A listing of packages by popularity is [here](https://hackage.haskell.org/packages/top).

Here are some project ideas:

* A data visualizer, reading in data with `cassava` and rendering using `gloss`.
* A basic game (Tetris, Snood, Breakout, Sokoban, etc) using `gloss`.
* A physics simulator using `gloss`.
* A data processing tool to clean up data from a science lab project, using `aeson` or `cassava`.
* A website that allows students to collect links to all 4 of their class syllabi, using `yesod`.
* A programming language refactoring tool, using `parsec`.

The challenge here will be to put your Haskell knowledge in a practical setting. These projects
should generally be modest in scope -- don't underestimate the challenge of learning a new,
large library.

Path 2: Types Types Types
-------------------------

Now that you've learned all about fancy types, it's time to put that knowledge to good use and
use them to verify a new algorithm. Much like how we used types to verify an insertion function
into a sorted list in [OrdList](../19/ClassOrdList.hs), so can you.

Here are some algorithms to consider:

* Sorting (the `OrdList` works *checks* a list for being sorted, but you can have it sort the
list instead)
* Binary search
* Binary search trees
* Algebraic manipulations to expressions (that is, augmenting Halgebra to do more, while asserting
that the value of an expression does not change as it is refactored)
* Regular expressions (with the types checking to make sure that a regex is well-formed)
* Blackjack (ensuring that a dealer makes the right move)
* Tic-Tac-Toe (ensuring that a player doesn't let the other player win in one move, unless this is impossible)
* [Rectangle tiling](https://pdfs.semanticscholar.org/8b79/2f78825bad68cf7a2267ea03db5b4273df33.pdf)
* Further verification of algorithms already worked with. (All the `Vec` examples reason only about
lengths. But you could verify other properties, too.)

Proposal
--------

Due by classtime Wednesday, April 19, the proposal will describe your project and show that you've
thought some about how you will achieve your goals. It will contain:

* a concrete statement of what you hope to achieve in your project. This includes the interface
of the program/functions that will comprise the main part of your project.

* a rationale of why this project is interesting to you. In other words, why
did you choose to do this particular project?

* a plan for what libraries (Path 1) or type definitions (Path 2) you expect to use. These are
not set in stone, but should be appropriate for your task. Your proposal must show some familiarity
with the library/types. (For types, I generally expect you will have to write your own.)

* details. A proposal should be detailed enough that another student (or I) could implement your
project and it would have the same outward-facing interface.

Proposals must be written in grammatical English. You will submit them on Gradescope in PDF format
*and* hand in a hard-copy in class on the due date. I expect they will be roughly 2 pages long, but
covering the content in enough detail is far more important than any length requirement.

I will review proposals quickly after submission to give you feedback on your choice of project.
This feedback may contain recommendations for a broadening or narrowing of the scope of your project
if, in my experience, I think it will be too easy/hard. (I'm much more likely to say "too hard" than
"too easy".)

Project
-------

The "Path" descriptions above pretty much say it all. Implement your project in idiomatic Haskell,
according to the specifications in your proposal.

Include in your project a README file that describes the project code, outlining where the major
functions are written and, in general, what I should look for when reviewing your work. This file
should also include reflections on the project process: What went well? What went poorly? What would
you do differently if you had more time?

Presentation
------------

On Monday, May 1, 2:00-5:00pm, we will gather for project presentations in Park 229. This is your
chance to demo your project and show off your work to the class. Presentations will be **5 minutes**
apiece. The presentation should include at least the following two parts:

* a demonstration of *what* the project does

* a walkthrough of *how* the project works

Presentations can be informal -- no slides required -- but you will need to project from your laptop
to show off your work.

Paper
-----

This paper, to be handed in on hard copy and uploaded to Gradescope by May 1, will be a reflection on
the experience of learning a typed, functional language. (It does not have to relate to the rest of
your project, specifically.) Contrast Haskell with other language(s) you know, letting these questions
guide you:
 * What is good in Haskell?
 * What is bad in Haskell?
 * Have you gained insight into programming in more traditional language by working
in Haskell?
 * If you could copy a feature from Haskell into another language, what feature? Why?
 * If you could copy a feature from another language into Haskell, what feature? Why?
 * If you have experience in an untyped language (e.g., Python, Scheme, JavaScript), what's
   your take on untyped vs. typed programming?

Your write-up should not be structured as a sequence of answers to these questions, but instead should
generally address these, and questions like them. In this paper, I am looking for a cogent explanation
of the issues at hand. I am *not* necessarily looking for you to agree with me on the points above!

I expect that it will take at least 2 pages to address this topic.

