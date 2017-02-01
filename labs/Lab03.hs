-- See http://www.seas.upenn.edu/~cis552/15fa/lectures/stub/SecretCode.html perhaps

-- wilmington exercises?

-- Lab03.hs
-- Names:
--
--
-- Read the problems below.
-- When you are done, upload your work in the lab03 directory at our
-- classwork repo, at https://github.com/bmc-cs380/classwork
-- Please rename the file before upload so you don't clobber your
-- classmates' work!

{- 1. Peer Review

   Taking turns, each member of your group should seek peer review about their
   solutions to hw01. Although scrutiny of any of hw01 is a good idea, make
   sure to discuss at least these functions:
     - maximumTableArea
     - containsStr
     - merge
     - mergeSorted

   Compare different styles and approaches to the problems. How did the others
   figure out the answers? Do any of you have lingering questions?

   During this process, remember that everyone in this class has a different
   level of experience and be supportive of one anothers' challenges. This is
   not a time to show off!

   I expect this will take at least 20 minutes to get through everyone's work.
-}

{- 2. List exercises

   Complete the List comprehension exercises from our syllabus page on the
   website (in the row for class 4).

   If you're having fun here, look up Project Euler problem #4, which is
   also solvable using a list comprehension.

-}

{- 3. Library functions

   Look up the following library functions on Hoogle to find their types and
   definitions. Then, reimplement them yourself.

     - mapMaybe
     - find
     - lookup
     - maybeToList
     - concatMap

-}

{- 4. Binary trees -}

-- Here is a binary tree type:
data Tree a where
  Leaf :: Tree a
  Node :: a      -- data
       -> Tree a -- left child
       -> Tree a -- right child
       -> Tree a
  deriving (Eq, Show)

-- Write a function that finds if an element is within a given tree:
elemTree :: Eq a => a -> Tree a -> Bool
elemTree = error "unimplemented"

-- Write a function that swaps the elements in a tuple in a tree.
-- Note that this does not swap left children with right children.
swapTree :: Tree (a,b) -> Tree (b,a)
swapTree = error "unimplemented"

-- Write a function that computes the depth of the tree: that is,
-- the largest number of Nodes to traverse on the way to a Leaf.
depth :: Tree a -> Int
depth = error "unimplemented"
