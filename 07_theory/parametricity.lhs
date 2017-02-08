% -*- mode: LaTeX -*-

\documentclass{article}

\usepackage{fullpage}
\usepackage{enumitem}
\usepackage{verbatim}
\usepackage{multicol}

\pagestyle{empty}

%include rae.fmt

\begin{document}

\begin{center}
\bf
CS380: Modern Functional Programming\\
Prof. Richard Eisenberg\\
Spring 2017\\[1ex]
Parametricity
\end{center}

For each of the problems below, write a total function with the given type. If
this is impossible, explain why.

\begin{multicols}{2}
\begin{enumerate}[itemsep=1in]
\item
\begin{code}
ex1 :: a -> b -> b
\end{code}

\item
\begin{code}
ex2 :: a -> a -> a
\end{code}

\item
\begin{code}
ex3 :: Int -> a -> a
\end{code}

\item
\begin{code}
ex4 :: Bool -> a -> a -> a
\end{code}

\item
\begin{code}
ex5 :: Bool -> Bool
\end{code}

\item
\begin{code}
ex6 :: (a -> a) -> a
\end{code}

\item
\begin{code}
ex7 :: (a -> a) -> a -> a
\end{code}

\item
\begin{code}
ex8 :: [a] -> [a]
\end{code}

\item
\begin{code}
ex9 :: (a -> b) -> [a] -> [b]
\end{code}

\item
\begin{code}
ex10 :: Maybe a -> a
\end{code}

\item
\begin{code}
ex11 :: a -> Maybe a
\end{code}

\item
\begin{code}
ex12 :: Maybe a -> Maybe a
\end{code}
\end{enumerate}
\end{multicols}
\end{document}
