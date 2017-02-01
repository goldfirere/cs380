{-# LANGUAGE GADTSyntax #-}

module Class where

import Prelude hiding ( Maybe(..), lookup, head )

{-
data Maybe a = Nothing | Just a
-}

data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a
  deriving (Eq, Ord, Show)

-- data OneOrTwo = One Int | Two Int Int
data OneOrTwo where
  One :: Int -> OneOrTwo
  Two :: Int -> Int -> OneOrTwo

data LotsOfStuff where
  MkLotsOfStuff :: String -> Int -> Bool -> Double -> LotsOfStuff

data Record where
  MkRecord :: { name :: String
              , value :: Int
              , isFun :: Bool
              , favoriteNumber :: Double } -> Record

add :: OneOrTwo -> Int
add (One n)   = n
add (Two n m) = n + m

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup x ( (p,q) : pairs )
  | (x == p)  = Just q
  | otherwise = lookup x pairs
lookup _ [] = Nothing

plusAssoc :: Int -> String -> [(String, Int)] -> Int
plusAssoc n s pairs
  = case lookup s pairs of
      Nothing -> n
      Just m  -> n + m

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

-- data List a = Nil | Cons a (List a)

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

myLength :: List a -> Int
myLength Nil = 0
myLength (Cons _ xs) = 1 + myLength xs
