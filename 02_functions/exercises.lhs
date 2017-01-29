% -*- mode: LaTeX -*-

\documentclass{article}

\usepackage{fullpage}
\usepackage{enumitem}
\usepackage{verbatim}

%include rae.fmt

\begin{document}

\begin{comment}
\begin{code}
import Prelude hiding ((++), concatMap)
\end{code}
\end{comment}

\begin{center}
\bf
CS380: Modern Functional Programming\\
Prof. Richard Eisenberg\\
Spring 2017\\[1ex]
Higher-Order Functions
\end{center}

Suppose the following functions have the types given:

\begin{spec}
frob :: Int -> String -> Bool
wurble :: Bool -> Int
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
($) :: (a -> b) -> a -> b
\end{spec}

Give types to each of the following (all considered separately), or write that the definition is ill-typed:

\begin{enumerate}
\item |f x = x|

|f ::|

\item |f x y = x y|

|f ::|

\item |f x y = y x|

|f ::|

\item |f (x:xs) = x|

|f ::|

\item |f (x:xs) = xs|

|f ::|

\item |f b = if b then b else b|

|f ::|

\item |f x y z = x (y z)|

|f ::|

\item |f = map wurble|

|f ::|

\item |f = map frob|

|f ::|

\item |f = filter wurble|

|f ::|

\item |f = filter frob|

|f ::|

\item |f = filter (wurble False)|

|f ::|

\item |f = filter (frob 5)|

|f ::|

\item |f = zipWith ($)|

|f ::|

\item |f x = filter (($) x)|

|f ::|

\item |f x = x x|

|f ::|

\end{enumerate}

We now assume the following definitions:

\begin{spec}
id :: a -> a
id x = x

const :: a -> b -> a
const a _ = a
\end{spec}

Figure out what the following reduce to, or say that the expression is ill-typed:

\begin{enumerate}[resume]
\item |map id [1,2,3]| $\longrightarrow$

\item |map (const False) ['x','y','z']| $\longrightarrow$

\item |filter (const False) "abc"| $\longrightarrow$

\item |filter id [True, False, True]| $\longrightarrow$

\item |zipWith id [id, not] [False, True]| $\longrightarrow$

\item |id not True| $\longrightarrow$

\item |id id 'x'| $\longrightarrow$

\end{enumerate}

Rewrite the following definitions into one-liners using |map| and |filter|:

\begin{enumerate}[resume]
\item 
\begin{spec}
f []      = []
f (x:xs)  = x + 1 : f xs
\end{spec}

\item
\begin{spec}
f []           = []
f (x:xs)
  | even x     = f xs
  | otherwise  = x : f xs
\end{spec}

\item
\begin{spec}
f []           = []
f (x:xs)
  | even x     = x `div` 2 : f xs
  | otherwise  = f xs
\end{spec}

\item
\begin{spec}
f []           = []
f (x:xs)
  | even y     = y : f xs
  | otherwise  = f xs
  where
    y = x `div` 2
\end{spec}

\end{enumerate}

Consider the following definitions:

\begin{code}
[] ++      ys  =  ys
(x:xs) ++  ys  =  x : (xs ++ ys)

concatMap _ []      = []
concatMap f (x:xs)  = f x ++ concatMap f xs
\end{code}

\begin{enumerate}[resume]
\item |(++) ::|
\item |concatMap ::|
\item Use |concatMap| to write a function |dup| that duplicates every element in a list. That is |dup ['x','y','z']| evaluates to |['x','x','y','y','z','z']|.
\end{enumerate}

\end{document}
