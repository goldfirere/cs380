{-# LANGUAGE NoMonomorphismRestriction #-}

module Functions where

{-
plusInt :: Int -> Int -> Int
plusFloat :: Float -> Float -> Float
plusInteger :: Integer -> Integer -> Integer
-}

maybePlus :: (Num a, Eq a) => a -> a -> a
maybePlus a b
  | a == b    = a + b
  | otherwise = 0

mult = (*)
plus = (+)

{-
map f [] = []
map f (x:xs) = f x : map f xs

(f . g) x = f (g x)
-}

id :: a -> a
-- id x = x

choose :: a -> a -> a
choose x _ = x
 -- OR
choose _ y = y

frob :: [Int] -> [Int]
frob = map (mult 2 . plus 1)
-- frob ns = map (\x -> x * 2) (map (\x -> x + 1) ns)

g :: Int -> Int -> Int
g x y = x + y * 2
-- g x = \y -> x + y * 2

h :: Int -> String -> Bool -> Char -> Double
h _ _ _ _ = 3.14
