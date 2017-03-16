---
title: CS 380 Policies
---

<div id="header">

| **CS 380 Course Policies**
| Prof. Richard Eisenberg
| Spring 2017

</div>

\$navbar\$

Grading
=======

<div id="grading_table">

----------------------   --------------
Assignments, projects       55%
Class contributions          5%
Quizzes                     10%
Exam 1                      10%
Exam 2                      10%
Final project               10%
<span class="strut" />
Total                       100%
----------------------   --------------

</div>

Assignment grading
------------------

All assignments are graded both for correctness and for style.

Correctness indicates how well the assignment meets its
specification -- that is, does it work? In correctness grading, I care more
about seeing your logic than about precisely what the program does when it
runs. For example, a program might have everything correct except for a
failed monadic pattern match early on. Such a
program will simply crash when run, but it is essentially correct. I might take
off just a bit of credit for the error, but you will earn the
rest of the credit for having an otherwise-correct program. The moral of this policy is this:
just because it doesn't work doesn't mean you've failed. Computers like to say
"right" and "wrong", but as a human, I can see the many points in between.

Style grading assesses how well you've conformed to the [Style
Guide](style.html), which dictates how you should write your code. Programming
is an act of communication both between you and a computer *and* between you and
another human. In some sense, correctness is about the former,
while style is about the latter.

Type-heavy assignments (second half of semester)     {#portfolios}
------------------------------------------------

When we start adding more and more types to our work, it becomes much easier to be
utterly stuck. It seems the stressful environment engendered by weekly assignment
deadlines makes this problem worse, not better. The second half of the semester
will thus use a *portfolio* model, where students accumulate a portfolio of type-correct
functions. These portfolios will be graded on a completion (and style) basis, toward
the end of the semester. (The exact date appears on the syllabus.)

There will be assignments with suggested deadlines. When you complete the assignment,
upload it to your portfolio repo on GitHub, at `https://github.com/bmc-cs380/`*\<your College
username\>*. As you do so, update the `README.md` file to reflect what assignment lives
in what file, and any comments you wish to make about the work there. Some assignments
have you figure out the right types to use. Typically, some time after these assignments
have been distributed, I will release the correct types. At this point, you should return
to your work to make sure that your types match mine. If you have the right type and
a non-trivial implementation, the implementation is almost surely correct -- which is
why we don't need to worry about correctness in this model.

Note that the deadlines are suggestions only. If you can't finish an assignment by the
deadline, do not fret. But do, perhaps, come to office hours.

If you want feedback on any aspect of your submitted work, just email. I will endeavor
to respond quickly!

Your portfolio will count for 25% of your total grade (slightly less than half of the
55% total for assignments / projects, above).

Class Contributions
-------------------

This component of your grade is a reflection of how you have contributed to
this class. It includes participation, attendance, and engagement. I expect
every student to contribute to the class environment, both to improve your own
experience and to improve the experience of others. For example, you can be an
active partner when working in groups, you can post on
[GitHub](https://github.com/bmc-cs380/cs380/issues) (where we will host our
question-and-answer forum) or on our
[mailing list](mailto:cs380-sp17@lists.cs.brynmawr.edu), you can raise your hand
in class, and you can visit my [office hours](index.html) -- but there are
other ways to contribute, as well.

A fantastic way to contribute is to find ways to improve this material. Submit
a [pull request](https://github.com/bmc-cs380/cs380/pulls) against the `cs380` repo!

Quizzes
-------

We will have a brief quiz every Wednesday during our lab time. These will be completed
online. Most quiz questions will ask you to determine the type of an expression
(or, later in the course, the kind of a type), although other questions are possible,
too. The quizzes will be open-note, but you will not be able to use any Haskell
tooling (e.g., `ghci`).

Exams
-----

This course has two exams, on March 1 and on April 24.
Exams will be open-book and
open-note, but you will not be able to use a computer.

More details will be discussed as the exams approach.

There will *not* be a final exam. Instead there will be a final project:

Final project
-------------

Toward the end of the semester, you will propose and start a *final project*, where
you will apply your newfound knowledge of functional programming and static typing
to some useful (or entertaining, or demonstrative) end. We will discuss details as
the final project gets closer.

In order for us to learn from others' travails, we will have *final project presentations*
during the exam period. (This is in lieu of a scheduled final exam and will appear on the
College schedule as a final exam. It is *not* a final exam!) These presentations are *not*
high-octane -- you will share what you've learned and show off some of your code. If you
want a slide or two, that's great, but the focus will be on the code, not on the polish.

You can choose to complete the final exam with up to one partner. Both students will work
together on the entire project and receive the same grade. (You will see below that there
is a three-assignment limitation to partners. This does *not* apply to the final project.)

Late policy
===========

Assignments are due by the beginning of class on the due date written on the
assignment. You will submit assignments via
[Gradescope](http://gradescope.com/) according to these [instructions](submitting.html).

Late assignments will lose 1 point (out of the 5-point scale) for every day
late (or portion thereof). Each student gets 3 free *late days* for the semester.
This means that the first three days (or portion thereof) that an assignment
is late will not lead to a penalty. These late days are intended to account for
unexpected bugs, minor illnesses, planned travel, etc. I will grant further
extensions only in rare circumstances.

Group work policy
=================

You are encouraged to work with others on assignments, **but your submission
must be your own**:

<div id="plagiarism">
All the code you submit must be written by you alone.
</div>

This means that, while it's a great idea to discuss general algorithms or
approaches with your classmates, **never share code**, and **never submit code
you found online**. Violators of this policy will be asked to report
themselves to the [Honor Board](http://sga.blogs.brynmawr.edu/honor-board/).

There is one exception to this rule: you may work with up to one partner on your
assignments, but you may use the same partner for no more than three assignments.
If you wish to work with a partner, you can register your partnership on Gradescope
as you submit your assignment.
When you are working with a partner,
both students get the same grade; naturally, the code you write is jointly yours,
slightly overriding the boldface policies above.

If you have a question, post on [GitHub](https://github.com/bmc-cs380/cs380/issues).

Installing Haskell
==================

Installing the Haskell toolchain (mostly, `ghc` and `stack`) can sometimes be challenging.
The instructions [here](fromhome.html) may be helpful. (Note: Do **not** do `brew install ghc` on
a Mac. It is out of date.) This is why we have installed everything you need on `powerpuff`.
While I can offer quick help in getting your personal machine working, it is not my
or the department's responsibility to get the tools working on your own machines, given
that you can log into `powerpuff` remotely and work there.

GitHub
======

This course will use [GitHub](http://github.com/) to host its question-and-answer
forum. If you have a question on an assignment, please post
[here](http://github.com/bmc-cs380/cs380/issues). If you look around, you will
also see that I host my own files used to run this course on GitHub. Feel free
to look at any of the materials there; in particular, you might find my class notes
(in the numbered folders) helpful. But be warned: these notes are written more for
me than for you, so your mileage may vary.

To post a question on our forum, you will need a GitHub account. These are free.
While I recommend associating your real name with your account, this is not necessary
if you prefer to be anonymous.

Accommodations for disability
=============================

Bryn Mawr College is committed to providing equal access to students with a
documented disability. Students needing academic accommodations for a
disability must first register with Access Services. Students can call
610-526-7516 to make an appointment with the Coordinator of Access Services,
Deb Alder, or [email her](mailto:dalder@brynmawr.edu) at `dalder@brynmawr.edu`
to begin this confidential process. Once registered, students should schedule
an appointment with me as early in the semester as possible to share the
verification form and make appropriate arrangements. Please note that
accommodations are not retroactive and require advance notice to implement.
More information can be obtained at the [Access Services
website](http://www.brynmawr.edu/access_services/).

Email
=====

I use email heavily as a way of communicating with students and colleagues.
Accordingly, I expect all my students to check email daily at their college
email address. There may be important announcements / corrections / other
messages there. Please read them!

As an avid emailer, I also am happy to receive email from all of you. During
business hours, you can expect a response from me within a few hours of your
email -- often much sooner. After 5pm or so, I tend to take a break from
technology for several hours, either until 9pm or even until the next morning.
Do *not* expect me to respond to an email in the evening. Similarly, entire
weekends go by without my checking my email (the nicer the weather, the less
chance of a response!), and so it's possible that something
you send on a Friday evening won't reach me until Monday morning.

In an emergency, you may call me at home at 484-344-5924 or on my cell at
201-575-6474. No texts, please.

Another small point about email: I have two Bryn Mawr email addresses:
`rae@cs.brynmawr.edu` and `raeisenber@brynmawr.edu`. Both email addresses
go to the same place, though, and both work. You do not need to worry about
which one you send to.

Meetings
========

My office hours for Spring 2017 are Tuesdays 2:30-3:30 and Wednesdays 1:15-2:30.
This means that, at
these hours, I am guaranteed to be in my office and expecting visitors -- and
I really do want visitors. During class, it's hard to get to know all of you,
and I'd love to know more about what brought you into my class, what else
you're interested in (in computer science and more broadly), and how your
college experience is going generally. Come with a question, come to say hi,
or come to play one of my puzzles. You can even use your curiosity about my
puzzle collection as an excuse to get in the door.

If you have a conflict with my office hours, please email so we can find another
time to meet.

Beyond my office hours, I aim to have an open-door policy. If you see my
office door (Park 204) open, please interrupt me: that's why the door is
open!

For a broader discussion than just homework questions, I'd be happy to join
you for lunch in the dining hall. Just email or ask!
