---
title: CS 380 Submission Instructions
---

<div id="header">

| **CS 380 Assignment Submission Instructions**
| Prof. Richard Eisenberg
| Spring 2017

</div>

\$navbar\$

TODO: Rewrite entire page.

All assignments in this course will be managed through GitHub, and you will be submitting
your assignments there. There are two sets of instructions below. The first is for you to
use on the first few assignments, before we have covered the intricacies of git. Thereafter,
I will expect you to use the second set of instructions, which will preserve your git
history.

A note on git history: git allows you to save intermediate, utterly broken, terribly
wrong, and otherwise embarrassing versions of your work. When you push your local repo
to GitHub, all this dirty laundry goes with it. Do not succumb to the temptation to clean
this away! Engineering is dirty work, filled with mistakes and silly notions. We all know
this. So it's OK for these intermediate steps to leak out. Yes, your final version should
be polished, but that's no reason not to also include your full git history.

To put my history where my mouth is, so to speak, my own repo for managing this course
is all [in public](https://github.com/bmc-cs246/cs246), with its full history, dead-ends,
notes to self, and all.

How to complete assignments before you know `git`
=================================================

Starting an assignment   {#start1}
----------------------

All assignments will have a "Click here to start assignment" link, which will lead you
to an invitation to start the assignment.
On this page, you will have to choose a team to be a part of. Because you can collaborate
with up to 1 other student on assignments, all assignments are completed by "teams".
If your partner has already started the assignment, choose the team name from the list
the appears. Otherwise (if you are working solo or if you are the first partner to start),
create a new team name. Your choice of team name is not important -- if you're feeling
particularly uncreative, you can just use your username.
(Students working solo will be in a team with 1 member.)


Accepting the invitation will create a new repo on GitHub with the starter
files for the assignment. It will be at `https://github.com/bmc-cs246/hwX-zzz`, where
the `Y` is replaced by the homework number and `zzz` is replaced by the team name you choose
when you accept the assignment. There will be a link to this repo from the page that comes
up when you accept the assignment.

From your repo page, there should be a green button labeled "Clone or download" toward the top-right
of the display. Click this and choose to download a zip file. On most computers, double-clicking
the downloaded zip file will unpack its contents. You can then edit the files to complete
the assignment.

How to submit   {#submit1}
-------------

When you are all with the assignment and ready to submit, you must complete two tasks:
*upload your files* and *create a pull request*.

1. *Upload your files*: Navigate back to your assignment repo page (something like
`https://github.com/bmc-cs246/hwY-zzz`). For every file you want to upload, navigate
in your web browser to the directoy that file belongs in and then choose "Upload files"
(a gray button two buttons to the left of that green one you used to download). You
may upload multiple files (to the same folder) at the same time. After uploading, click
on the filenames in your web browser and make sure that you see your edits.

2. *Create a pull request*: Click the "New pull request" button toward the left, above
all the files. You will be brought to a "Compare changes" screen. Just a bit below
the "Compare changes" title, there should be two pull-down menus, one labeled "base"
and one labeled "compare". In the *base* menu, choose `pristine`. After you make this
selection, you should see your changes appear below on that same page, with the
starter code on the left and your version on the right. This is the view I will be
seeing and will grade.

    If everything looks good, give a title to your submission ("hw2 submission" is
    just fine, but feel free to be creative here) and write any comments you want me to
    see while grading in the comment box. Click the green "Create pull request" button.
    You will get a pull request number (often, `#1`). When you see that, your assignment
    is submitted.

If you want to resubmit
-----------------------

GitHub continues to track changes even after you create a pull request. So you can continue
to upload files to the repo even after you create the pull request and the pull request is
automatically updated. Thus there is no specific step you must take to resubmit.

How to complete assignments with `git`
======================================

Register an SSH key with GitHub
-------------------------------

Before pushing your local repo to GitHub will work, you need to register your SSH (Secure SHell)
key with GitHub.

1. Do you have an SSH key?
  a. If you don't have a `~/.ssh` directory, you don't have an SSH key. Go to step (c).
  b. If you do have a `~/.ssh` directory, go there. See if you have an `id_rsa.pub` file.
     If you do, you have an SSH key and can proceed to register it with GitHub. Otherwise,
     go to step (c).
  c. Create an SSH key by completing Steps One and Two from [this page](https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys--2).

2. Follow [these instructions](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/) to register the SSH key.

You will have to register a key separately from each computer you will push repos from.

Starting an assignment
----------------------

As in the first two paragraphs [above](#start1), accept the invitation for the assignment
via the link in the assignment.

Then, you can clone the repo with `git clone ssh://git@github.com/bmc-cs246/hwY-zzz` where
`hwY-zzz` is the name of your repo. Edit files and commit to your heart's content. You
can even `git push` before your down to create a backup copy of your work.

How to submit
-------------

1. `git push`. This should copy all your edited files onto GitHub.

2. Follow step (2) [above](#submit1).

If you want to resubmit
-----------------------

`git push`
