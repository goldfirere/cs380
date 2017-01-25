% -*- mode: LaTeX -*-

\documentclass{article}

\usepackage{fullpage}
\usepackage{enumitem}
\usepackage{verbatim}

%include rae.fmt

\setlist{itemsep=8ex}

\begin{document}

\begin{center}
\bf
CS380: Modern Functional Programming\\
Prof. Richard Eisenberg\\
Spring 2017\\[1ex]
Quiz 0
\end{center}

Suppose the following functions have the types given:

\begin{spec}
map :: (a -> b) -> [a] -> [b]
plus :: Int -> Int -> Int
not :: Bool -> Bool
idBool :: Bool -> Bool
\end{spec}

Give types to each of the following (all considered separately), or write that the definition is ill-typed. You may assume that all numbers are |Int|s, if you like.

\begin{enumerate}
\item |plus 3 4|
\item |plus 3|
\item |[1,2,3]|
\item |True : []|
\item |[True, 3]|
\item |map not|
\item |map plus|
\item |not map|
\item |[False, not]|
\item |[not, idBool]|
\end{enumerate}

\end{document}
