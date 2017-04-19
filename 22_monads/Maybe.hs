
import Prelude hiding (Maybe (..))
import Control.Monad ( guard )

data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure x = Just x
  Just f <*> Just x = Just (f x)
  _ <*> _ = Nothing

instance Monad Maybe where
  Just a >>= f = f a
  Nothing >>= _ = Nothing

isPrime n = null [ x | x <- [2 .. n-1], n `mod` x == 0 ]

factors n = do
  fact1 <- [ 1 .. n-1 ]
  fact2 <- [ 1 .. n-1 ]
  guard (fact1 * fact2 == n)
  return (fact1, fact2)
