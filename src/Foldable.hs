module Foldable where
import Data.Monoid
import Data.Foldable()
import GHC.Base()
import Data.Semigroup ( Max(Max, getMax), Min(Min, getMin) )

-- 1. Implement fold in terms of foldMap.

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 2. What would you need in order to implement foldMap in terms of fold?

-- foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap'' f ta = undefined

-- 3. Implement foldMap in terms of foldr.

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> f x <> y) mempty

-- 4. Implement foldr in terms of foldMap (hint: use the Endo monoid).

foldr'   :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f z xs = appEndo (foldMap (Endo . f) xs) z

-- 5. What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?

-- 6. Implement toList :: Foldable f => f a -> [a] in terms of either foldr or foldMap.
toList' :: Foldable f => f a -> [a]
toList' = foldMap (: [])

-- 7. Show how one could implement the generic version of foldr in terms of toList, assuming we had 
--    only the list-specific foldr :: (a -> b -> b) -> b -> [a] -> b.


-- 8. Pick some of the following functions to implement: concat, concatMap, and, or, any, all, sum, 
--    product, maximum(By), minimum(By), elem, notElem, and find. Figure out how they generalize to 
--    Foldable and come up with elegant implementations using fold or foldMap along with appropriate 
--    Monoid instances.

concat' :: Foldable t => t [a] -> [a]
concat' = foldMap (++ [])

concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' f = foldMap ((++ []) . f)

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

any :: Foldable t => (a -> Bool) -> t a -> Bool
any f = getAny . foldMap (Any . f)

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f = getAll . foldMap (All . f)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

maximum :: (Ord a, Bounded a, Foldable t) => t a -> a
maximum = getMax . foldMap Max

minimum :: (Ord a, Bounded a, Foldable t) => t a -> a
minimum = getMin . foldMap Min

-- maximumBy :: (Foldable t, Ord a, Bounded a) => (a -> a -> Ordering) -> t a -> a

-- minimumBy :: (Foldable t, Ord a, Bounded a) => (a -> a -> Ordering) -> t a -> a

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem' x = foldr (\a b -> b && a == x) False
elem' x = getAny . foldMap (Any . (== x))


notElem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- notElem' x = foldr (\a b -> b && a /= x) True
notElem' x = getAll . foldMap (All . (/= x))

find' :: Foldable t => (a -> Bool) -> t a -> Maybe a 
find' p = getFirst . foldMap (\x -> First (if p x then Just x else Nothing))

-- 9. Implement traverse_ in terms of sequenceA_ and vice versa. One of these will need an 
--    extra constraint. What is it?

-- traverse_' :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
-- traverse_' f xs = 