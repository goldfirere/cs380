{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, GADTSyntax,
             FlexibleContexts #-}

module Class where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

x :: T.Text
x = "hi"

never_ever :: IO a -> a
never_ever = error "terrible"

action :: IO ()
action = do
  filedata <- B.readFile "markets.json"
  print (B.length filedata)

data OrdList a where
  OrdList :: { ordering :: a -> a -> Ordering
             , stuff :: [a] } -> OrdList a
--  SomeOtherConstructor :: Int -> OrdList a   <-- usually a bad idea
{-
data OrdByYCoordinate where
  OBYC :: Market -> OrdByYCoordinate

instance Ord OrdByYCoordinate where
-}

f x = [ z | z <- [1..10], x z ]
f' x = filter x [1..10]
-- f :: (a -> b) -> [b]     <-- RETRACTED
-- f :: (Int -> b) -> [b]   <-- RETRACTED
f :: (Int -> Bool) -> [Int]

-- this one has better style:
len []     = 0
len (_:xs) = 1 + len xs

len' list
  | null list = 0
  | otherwise = 1 + len' (tail list)

len'' list = case list of
  []   -> 0
  _:xs -> 1 + len'' xs

divide :: (Integral a) => a -> a -> Maybe a
divide x y
  | y == 0    = Nothing
  | otherwise = Just $ x `div` y

divide_and_show :: (Integral a, Show a) => a -> a -> Maybe String
divide_and_show a b = case a `divide` b of
  Just quot -> Just $ show quot
  Nothing   -> Nothing
