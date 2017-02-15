module Main where

import System.IO

main = do
  putStrLn "Hello, world!"
  putStr "Enter your name: "
  hFlush stdout
  name <- getLine
  putStrLn ("Hello, " ++ name)
