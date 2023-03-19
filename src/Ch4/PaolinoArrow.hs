{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ch4.PaolinoArrow (main) where

import Control.Arrow (Arrow ((&&&)), arr, returnA)
import Control.Category (Category ((.), id))
import qualified Control.Foldl as F
import qualified Control.Foldl.NonEmpty as F1
import Control.Monad (join)
import Control.Monad.State.Strict (MonadState (state))
import qualified Control.Scanl as S
import Data.Attoparsec.ByteString.Char8 (decimal, parseOnly, skipSpace, string)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv (FromField (..), HasHeader (..))
import Data.Csv.Streaming (decode)
import Data.Function (on)
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup (First (..), Sum (Sum))
import Prelude hiding ((.), id)

makeScan :: s -> (a -> s -> (b, s)) -> S.Scan a b
makeScan s0 f = S.Scan (state . f) s0

-- hold the element that passes the check
hold ::
    -- | check
    (a -> a -> Bool) ->
    S.Scan a a
hold op = makeScan Nothing f
  where
    f y (Just x) = if y `op` x then (y, Just y) else (x, Just x)
    f y Nothing = (y, Just y)

-- lift hold to operate on Maybes. Nothing elements will not touch the state
holdOnJust ::
    -- | check
    (a -> a -> Bool) ->
    S.Scan (Maybe a) (Maybe a)
holdOnJust op = hold f
  where
    f Nothing _ = False
    f _ Nothing = True
    f (Just x) (Just y) = op x y

-- compute the sliding windows of given elements,
-- return Nothing for the first windows until they do not have enough elements
slidingWindow ::
    -- | size
    Int ->
    S.Scan a (Maybe [a])
slidingWindow l = makeScan (1, []) $ \x (n, xs) ->
    let xs' = take l $ x : xs
        r =
            if n >= l
                then Just xs'
                else Nothing
     in (r, (n + 1, xs'))

-- compute the average of the list of R together with the first date in the list
-- notice the use of F1 module to require only Semigroup in the fold
-- checkout Semigroup.First as opposed to Monoid.First
average :: [R] -> Maybe R
average xs = do
    ys <- nonEmpty xs
    let (First d, Sum n, Sum s) =
            F1.fold1 F1.sconcat $
                fmap (\(d', t) -> (First d', Sum @Int 1, Sum t)) ys
    pure (d, s / fromIntegral n)

-- year month day
data Date = Date Int Int Int
    deriving (Show, Ord, Eq)

-- parse a date
parseDate :: ByteString -> Either String Date
parseDate = parseOnly $ do
    skipSpace
    year <- decimal <* string "-"
    month <- decimal <* string "-"
    Date year month <$> decimal

-- necessary to use Date as a field on parsing a CSV content
instance FromField Date where
    parseField = either fail pure . parseDate

-- our record with date and temp
type R = (Date, Float)

main :: IO ()
main = do
    r <- BL.readFile "temperatures.csv"
    mapM_ print $ -- skip Nothing
        join $ -- join Maybes
            F.fold F.last $ -- compute the last
                S.scan scanDatesArrowSyntax $
                    decode NoHeader r -- streaming of elements of type R

{-
leverage Scan instances to compute min and max in parallel
    after computing the average

the shape here is

> fmap f (x &&& y) . fmap g y

the '.' is coming from Category and it's composing to Scan's vertically

the '&&&' is coming from Arrow and it's composing 2 Scan horizontally so that
they consume the same input and produce a tuple of the outputs

notice that also (a -> b) is a Category and an Arrow

notice that 'Scan a b' as well as '(a -> b)' are profunctors so that we can
    'lmap' as well as 'rmap' (isomorphic to 'fmap').
    We are not using it in this code, but could be useful in case you need to
    operate on the input of a Scan
-}

scanDatesPlain :: S.Scan R (Maybe (Date, Date))
scanDatesPlain =
    fmap
        (\(minH, maxH) -> (,) <$> (fst <$> minH) <*> (fst <$> maxH))
        (holdOnJust ((<) `on` snd) &&& holdOnJust ((>) `on` snd))
        . fmap (>>= average) (slidingWindow 3)

-- same function with arrow syntax, nice one to avoid (&&&) by naming the input
-- and supplying it twice.
-- returnA is the identity Arrow 
scanDatesArrowSyntax :: S.Scan R (Maybe (Date, Date))
scanDatesArrowSyntax = proc xs -> do
    ws <- slidingWindow 3 -< xs
    as <- arr (>>= average) -< ws -- let as = ws >>= average
    minH <- holdOnJust ((<) `on` snd) -< as
    maxH <- holdOnJust ((>) `on` snd) -< as
    id -< (,) <$> (fst <$> minH) <*> (fst <$> maxH)

-- exercise: implement \k f g h x -> h (f (k x), g (k x)) with Arrow notation

exercise :: (a -> b) -> (b -> c) -> (b -> d) -> ((c, d) -> e) -> a -> e
exercise k f g h = proc x -> do 
    returnA -< error "implement me"
