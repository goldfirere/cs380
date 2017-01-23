---
title: "CS 380: Working from home"
---

<div id="header">

| **CS 380: How to work from home**
| Prof. Richard Eisenberg
| Spring 2017

</div>

\$navbar\$

Regardless of where you work, it is assumed that you have some facility working at a command
line. If you don't you may want to check out [The Linux Command Line: A Complete Introduction](https://catalog.tricolib.brynmawr.edu/find/Record/.b3852686), available online from libraries
in all three campuses.

This course is designed around the software that is available in the CS labs. (CS labs at both campuses have the requisite
software.) You may have the best experience working at the lab computers. However, if that is not feasible, it is
very possible to work from home. This page has some instructions on how to do this effectively.

From a Mac, working locally
---------------------------

Mac OS X is actually a variant of UNIX, just like Linux is a variant of UNIX.
This means that you will be able to complete your assignments on your computer
quite nicely. To get to the command line, just launch the *Terminal* app (in
the *Utilities* folder under *Applications*, or available via Spotlight).

You will need the *Haskell Platform Minimal*, available [here](https://www.haskell.org/platform/#osx) (click the button labeled "Core"). After installing this, you should be able to
run GHC from the Terminal. Try typing `ghci`.

You will also likely want some Haskell gubbins for your editor. Various editors have various
levels of Haskell integration; emacs and Atom seem to have the best. Searching online for
how to set these up with Haskell works nicely.

From a Mac, working remotely
----------------------------

If you don't want to worry about setting software up on your own machine, you
are welcome to connect to Bryn Mawr's CS server, `powerpuff`. Just open
*Terminal* and say `ssh `*<your CS username>*`@powerpuff.cs.brynmawr.edu`. You
will then have command-line access to the CS server, from where you can run
`emacs` and start programming.

When you need to copy files back and forth between your machine and the
server, you can use `scp`.

See also the last section here about some advice about working remotely.

From Windows, working locally
-----------------------------

Windows does not have nearly the power of a Mac when it comes to programming.
It is *possible* to get everything working on Windows, but it's not easy.
Given the wide range of situations out there, all I can do here is recommend a
few starting points -- it will be up to you to get all the software working,
I'm afraid.

You should install [MinGW](http://www.mingw.org/) and [MSYS](http://www.mingw.org/wiki/msys)
(or you could use the heavier-weight [Cygwin](https://www.cygwin.com)) to get a
working command line. Then, install the [Haskell Platform Minimal](https://www.haskell.org/platform/#windows) (a.k.a. Core). You should now be able to run `ghci` from the command line.

From Windows, working remotely
------------------------------

Windows also does not come with a built-in `ssh` (Secure SHell) client.
Download [PuTTY](www.chiark.greenend.org.uk/~sgtatham/putty/download.html),
both the `putty.exe` and `pscp.exe` files. Launching `putty.exe`, you will be
able to configure a connection to `powerpuff.cs.brynmawr.edu`, using your CS
username. It is worthwhile looking through the other settings available, as
tweaking them may prove fruitful. The `pscp.exe` program will allow you to
copy files back and forth between your computer and the server.

Once you have logged in with PuTTY, you should be able to run `emacs` or `ghci`, and away you go.

See also below for some advice about working remotely.

Helpful commands when working remotely
--------------------------------------

When working via `ssh` or PuTTY, you do not have direct access to the files
you are working with. (For example, you can't just save files from web pages
into the server.) While you can always use `scp` or `pscp.exe`, this is often
inconvenient. Below are some tips for common scenarios:

- Use multiple windows! It is always possible to have multiple `ssh` or PuTTY
sessions at the same time. This is much easier than exiting emacs every time
you wish to compile!

- If you need to download a file from the web, from URL
  http://example.com/file.txt, use `wget http://example.com/file.txt`.

- Some terminals have a hard time with emacs's `M-` key. So any `M-blah`
command in emacs can also be executed by typing `ESC blah`. So, instead of
`M-x`, you would type `ESC x`. Note that `ESC` and `x` are *not* typed at the
same time here.

- Check out the [emacs reference card](resources/emacs-refcard.pdf).

Have a tip to add? Create a pull request!
