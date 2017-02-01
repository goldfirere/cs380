% -*- mode: LaTeX -*-

\documentclass{article}

\usepackage{fullpage}
\usepackage{enumitem}
\usepackage{verbatim}
\usepackage{multicol}

%include rae.fmt

\setlist{itemsep=8ex}
\setlength{\parindent}{0pt}

\begin{document}

\pagestyle{empty}

\begin{center}
\bf
CS380: Modern Functional Programming\\
Prof. Richard Eisenberg\\
Spring 2017\\[1ex]
Quiz 1
\end{center}

Suppose the following functions have the types given:

\begin{multicols}{2}
\begin{spec}
map     :: (a -> b) -> [a] -> [b]
filter  :: (a -> Bool) -> [a] -> [a]
plus    :: Int -> Int -> Int
not      :: Bool -> Bool
toupper  :: Char -> Char
(<)      :: Int -> Int -> Int
\end{spec}
\end{multicols}

Give types to each of the following expressions, or write that the expression is ill-typed. You may assume that all numbers are |Int|s, if you like.

\begin{enumerate}
\item |plus 3 4|
\item |plus 3|
\item |[1,2,3]|
\item |True : []|
\item |[True, 3]|
\item |map not|
\item |[ x || x <- "hello", toupper x ]|
\item |(< 3)|
\item |filter not|
\item |filter not [True]|
\end{enumerate}

\end{document}
