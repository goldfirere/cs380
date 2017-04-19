-- Eval.hs
-- Defines evaluation over arithmetic expressions

{-# LANGUAGE GADTSyntax #-}

module Eval where

import Control.Monad ( when )

import NatVec
import Arith
import Parser   -- just for ease of us in GHCi

-- Basic evaluator
evalSum :: Vec n Double -> Sum n -> Double
evalSum e (Plus a b)  = evalSum e a + evalSum e b
evalSum e (Minus a b) = evalSum e a - evalSum e b
evalSum e (Term a)    = evalTerm e a

evalTerm :: Vec n Double -> Term n -> Double
evalTerm e (Mult a b) = evalTerm e a * evalTerm e b
evalTerm e (Div a b)  = evalTerm e a / evalTerm e b
evalTerm e (Factor f) = evalFactor e f

evalFactor :: Vec n Double -> Factor n -> Double
evalFactor _ (Lit n) = fromInteger n
evalFactor e (Var f) = e !!> f
evalFactor e (Sum s) = evalSum e s

-- The Reader monad, which performs a computation with access to an environment
data Reader env a where
  Reader :: (env -> a) -> Reader env a

instance Functor (Reader env) where
  fmap f (Reader fun) = Reader (\env -> f (fun env))

instance Applicative (Reader env) where
  pure x                    = Reader $ \_ -> x
  (Reader f) <*> (Reader x) = Reader $ \env -> f env (x env)

instance Monad (Reader env) where
  Reader a >>= f = Reader $ \env ->
    case f (a env) of
      Reader b -> b env

-- Retrieve the environment in a Reader
get :: Reader env env
get = Reader id

-- Execute a Reader monad
runReader :: env -> Reader env a -> a
runReader env (Reader f) = f env

-- Evaluator with a Reader monad, where the variable values are in the env
evalSumR :: Sum n -> Reader (Vec n Double) Double
evalSumR (Plus a b) = do
  a' <- evalSumR a
  b' <- evalSumR b
  return (a' + b')
evalSumR (Minus a b) = do
  a' <- evalSumR a
  b' <- evalSumR b
  return (a' - b')
evalSumR (Term a) = evalTermR a

evalTermR :: Term n -> Reader (Vec n Double) Double
evalTermR (Mult a b) = do
  a' <- evalTermR a
  b' <- evalTermR b
  return (a' * b')
evalTermR (Div a b) = do
  a' <- evalTermR a
  b' <- evalTermR b
  return (a' / b')
evalTermR (Factor f) = evalFactorR f

evalFactorR :: Factor n -> Reader (Vec n Double) Double
-- evalFactorR :: Factor n -> (Vec n Double -> Double)
evalFactorR (Lit n) = return (fromInteger n)
evalFactorR (Var fin) = do
  env <- get
  return (env !!> fin)
evalFactorR (Sum s) = evalSumR s

-- The Counter monad, which performs a computation that can count
data Counter a where
  Counter :: (Int, a) -> Counter a

instance Functor Counter where
  fmap f (Counter (n, a)) = Counter (n, f a)

instance Applicative Counter where
  pure x                                  = Counter (0, x)
  (Counter (n1, f)) <*> (Counter (n2, x)) = Counter (n1 + n2, f x)

instance Monad Counter where
  (Counter (n1, a)) >>= f =
    case f a of
      Counter (n2, b) -> Counter (n1 + n2, b)

-- Make the counter count
count :: Counter ()
count = Counter (1, ())

-- Run a Counter computation, returning the result and the count
runCounter :: Counter a -> (Int, a)
runCounter (Counter pair) = pair

-- Evaluator with a Counter monad, counting the number of trivial operations
evalSumC :: Vec n Double -> Sum n -> Counter Double
evalSumC e (Plus a b) = do
  a' <- evalSumC e a
  b' <- evalSumC e b
  when (a' == 0 || b' == 0) count
  return (a' + b')
evalSumC e (Minus a b) = do
  {
    a' <- evalSumC e a;
    b' <- evalSumC e b;
    when (b' == 0)
      count;
    return (a' - b');
  }
evalSumC e (Term a) = evalTermC e a

evalTermC :: Vec n Double -> Term n -> Counter Double
evalTermC e (Mult a b) = do
  a' <- evalTermC e a
  b' <- evalTermC e b
  when (a' == 1 || b' == 1) count
  return (a' * b')
evalTermC e (Div a b) = do
  a' <- evalTermC e a
  b' <- evalTermC e b
  when (b' == 1) count
  return (a' / b')
evalTermC e (Factor f) = evalFactorC e f

evalFactorC :: Vec n Double -> Factor n -> Counter Double
evalFactorC _ (Lit n)   = return (fromInteger n)
evalFactorC e (Var fin) = return (e !!> fin)
evalFactorC e (Sum s)   = evalSumC e s
