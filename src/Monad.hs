{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad where

import Applicative
import Data.List ((++))
import Data.Function ((.))
import Data.Functor
import Prelude (id, ($))

class Applicative m => Monad' m where
  return :: a -> m a
  return = pure
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  m >> n = m >>= \_ -> n
  join :: m (m a) -> m a
  join x = x >>= id

-- 1. Implement a Monad instance for the list constructor, []. Follow the types!

instance Applicative [] => Monad' [] where
    (>>=) [] _ = []
    (>>=) (x:xs) f = f x ++ (xs >>= f)

-- 2. Implement a Monad instance for ((->) e).

instance Applicative ((->) e) => Monad' ((->) e) where
    k >>= f = \x -> (f . k) x x


-- 3. Implement Functor and Monad instances for Free f, defined as
data Free f a = Var a | Node (f (Free f a))

instance (Functor f, Applicative (Free f)) => Monad' (Free f) where
    Var x >>= f = f x
    Node x >>= f = Node ((>>= f) <$> x)

--    You may assume that f has a Functor instance. This is known as the free monad built from the functor f.

-- 4. Implement (>>=) in terms of fmap (or liftM) and join.

(>>@) :: Monad' m => m a -> (a -> m b) -> m b
k >>@ f = join $ fmap f k

-- 5. Now implement join and fmap (liftM) in terms of (>>=) and return.

join' :: Monad' m => m (m a) -> m a
join' x = x >>= id

fmap' :: Monad' m => (a -> b) -> m a -> m b
fmap' f ma = ma >>= (return . f)

-- Monad Laws:
-- return a >>= k           =  k a
-- m >>= return             =  m
-- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

-- In term of >=> ( (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c ):
-- return >=> g     =  g
-- g >=> return     =  g
-- (g >=> h) >=> k  =  g >=> (h >=> k)

-- 5. Given the definition g >=> h = \x -> g x >>= h, prove the equivalence of the above laws and the usual monad laws.

(>=>) :: Monad' m => (t -> m a) -> (a -> m b) -> t -> m b
g >=> h = \x -> g x >>= h

q :: Monad' m => a -> (a -> m b) -> m b
q a k = return a >>= k

r :: Monad' m => m b -> m b
r m = m >>= return

t :: Monad' m => m t -> (t -> m a) -> (a -> m b) -> m b
t m k h = m >>= (\x -> k x >>= h)


q' :: Monad' m => (a -> m b) -> a -> m b
q' g = return >=> g -- \x -> return x >>= g --> return x >>= g

r' :: Monad' m => (a -> m b) -> a -> m b
r' g = g >=> return -- \x -> g x >>= return -->> m >>= return

t' :: Monad' m => (t -> m a1) -> (a1 -> m a2) -> (a2 -> m b) -> t -> m b
t' g h k = (g >=> h) >=> k -- \x -> (\x' -> g x' >>= h) x >>= k --> (\x' -> g x' >>= h) x >>= k

t'' :: Monad' m => (t -> m a1) -> (a1 -> m a2) -> (a2 -> m b) -> t -> m b
t'' g h k = g >=> (h >=> k) -- \x -> g x >>= (\x' -> h x' >>= k) --> g >>= (\x' -> h x' >>= k) --> g >>= (h >>= k)