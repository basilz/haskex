{-# LANGUAGE NoImplicitPrelude #-}

module Functors where

import Data.Int
import Control.Category ((.))
import GHC.Show ( Show )
import Data.Eq

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- 1. Implement Functor instances for Either e and ((->) e).

data Either a b = Left a | Right b

instance Functor (Either e) where
    fmap f (Right r) = Right (f r)
    fmap _ (Left l) = Left l

instance Functor ((->) e) where
    fmap = (.)


-- 2. Implement Functor instances for ((,) e) and for Pair, defined as
--    data Pair a = Pair a a

instance Functor ((,) e) where
    fmap f (e,x) = (e, f x)


data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- Explain their similarities and differences.

-- 3. Implement a Functor instance for the type ITree, defined as

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
    fmap f (Leaf fia) = Leaf (f . fia)
    fmap _ (Node []) = Node []
    fmap f (Node tts) = Node (go f tts)
        where
            go _ [] = []
            go f' (t:ts) = fmap f' t:go f' ts

-- 4. Give an example of a type of kind * -> * which cannot be made an instance of Functor 
--    (without using undefined).

newtype T a = T (Int -> a)

-- 5. Is this statement true or false?
--    The composition of two Functors is also a Functor. 
--    If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

-- True, here is an example of composition of functors which is also a functor:

newtype Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap h (Compose x) = Compose (fmap (fmap h) x)

-- 6. Although it is not possible for a Functor instance to satisfy the first Functor law but not the 
--    second (excluding undefined), the reverse is possible. Give an example of a (bogus) Functor 
--    instance which satisfies the second law but not the first.

data Bogus a = Bigus | Bigul deriving (Show, Eq)

instance Functor Bogus where
    fmap _ _ = Bigul

-- 7. Which laws are violated by the evil Functor instance for list shown above: both laws, or the first 
--    law alone? Give specific counterexamples.
--
--    Evil Functor instance:
--
instance Functor [] where
     fmap _ [] = []
     fmap g (x:xs) = g x : g x : fmap g xs

-- it violates both