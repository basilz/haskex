module Change (test, countChangeOpen, countChangeClosed) where

import Control.Exception.Base (assert)
import Data.List (sortOn)
import Data.Ord (Down (..))

-- count the number of ways a `tot` can be realized using the given coins
-- any coin can be used any number of times
countChangeOpen :: Int -> [Int] -> Int
countChangeOpen tot coins = consumeOpen tot $ sortOn Down coins

consumeOpen ::
  Int -> -- tot to realize
  [Int] -> -- coin values
  Int -- ways
consumeOpen 0 _ = 1
consumeOpen r [c] = if r < c then 0 else 1
consumeOpen r ccs@(c : cs)
  | r > 0 = consumeOpen (r - c) ccs + consumeOpen r cs
  | otherwise = 0
consumeOpen _ _ = 0

-- count the number of ways a `tot` can be realized using the given coins in limited number
countChangeClosed ::
  Int -> -- tot to realize
  [(Int, Int)] -> -- list of (count, coin value)
  Int -- ways
countChangeClosed tot coins = consumeClosed tot $ sortOn (Down . snd) coins

consumeClosed :: Int -> [(Int, Int)] -> Int
consumeClosed 0 [] = 1
consumeClosed r ((0, c) : cs) = consumeClosed (r - c) cs
consumeClosed r [(n, c)] = if r < c then 0 else 1
consumeClosed r ((n, c) : cs) 
    | r > 0 = consumeClosed (r - c) ((n - 1, c) : cs) + consumeClosed r cs
    | otherwise = 0
consumeClosed _ _ = 0

test :: IO ()
test = do
  test $ countChangeOpen 100 [1, 2, 5, 30] == 922
  test $ countChangeClosed 100 [(30, 1), (20, 2), (10, 5), (3, 30)] == 241
  where
    test = flip assert mempty
