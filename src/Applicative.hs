{-# LANGUAGE NoImplicitPrelude #-}

module Applicative where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Function (flip, ($), (.))
import Data.List (repeat, zipWith)
import Data.Maybe
import GHC.Show (Show)
import Prelude (const)
import Data.Tuple (uncurry)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- The identity law:
-- pure id <*> v = v

-- Homomorphism:
-- pure f <*> pure x = pure (f x)

-- Interchange:
-- u <*> pure y = pure (\f -> f y) <*> u

-- Composition:
-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

-- 1. (Tricky) One might imagine a variant of the interchange law that says something
--    about applying a pure function to an effectful argument. Using the above laws,
--    prove that:
--    pure f <*> x = pure (flip ($)) <*> x <*> pure f
q x f = pure (flip ($)) <*> x <*> pure f

q' x f = (pure (flip ($)) <*> x) <*> pure f

q'' x f = pure ($ f) <*> (pure (flip ($)) <*> x)

q''' x f = pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x

q'''' x f = pure ((.) ($ f)) <*> pure (flip ($)) <*> x

q''''' x f = pure (($ f) . (flip ($))) <*> x

q'''''' x f = pure f <*> x

-- 2. Implement an instance of Applicative for Maybe.

instance Applicative Maybe where
  pure x = Just x
  (<*>) (Just f) (Just x) = Just (f x)
  (<*>) _ _ = Nothing

-- 3. Determine the correct definition of pure for the ZipList instance of Applicative
-- —there is only one implementation that satisfies the law relating pure and (<*>).

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
  pure x = ZipList $ repeat x
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

-- 4. Implement a function:

-- sequenceAL :: Applicative f => [f a] -> f [a]

-- There is a generalized version of this, sequenceA, which works for any Traversable
-- (see the later section on Traversable), but implementing this version specialized
-- to lists is a good exercise.

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x : xs) = ((:) <$> x) <*> sequenceAL xs
--sequenceAL = foldr (\x -> (<*>) ((:) <$> x)) (pure [])

-- 5. Implement pure and (<*>) in terms of unit and (**), and vice versa.

class Functor f => Monoidal f where
  unit :: f ()
  unit = pure' ()
  (**) :: f a -> f b -> f (a, b)
  (**) fa fb = pure' (,) <@> fa <@> fb
  pure' :: a -> f a
  pure' x = (const x) <$> unit
  (<@>) :: f (a -> b) -> f a -> f b
  (<@>) fab fa = uncurry ($) <$> (fab ** fa)

-- 6. Are there any Applicative instances for which there are also functions
--    f () -> () and f (a,b) -> (f a, f b), satisfying some "reasonable" laws?

instance Monoidal Maybe where
  unit = Just ()
  Just x ** Just y = Just (x, y)
  _ ** _ = Nothing

-- unit ** Just x = ((), x) ~= x
-- unit ** Nothing = Nothing 
-- Just x ** unit = (x, ()) ~= x
-- Nothing ** unit = Nothing
-- Just x ** (Just y ** Just z) = Just x ** Just (y, z) = Just (x, (y, z)) ~= (x, (y, z)) ~= ((x, y), z)
-- Nothing ** (Just y ** Just z) = Nothing = (Nothing ** Just y) ** Just z
-- ecc.

-- 7. (Tricky) Prove that given your implementations from the first exercise, the
--    usual Applicative laws and the Monoidal laws stated above are equivalent.
