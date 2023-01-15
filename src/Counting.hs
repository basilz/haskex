{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Counting where

-- some values
type S a = [a]

-- any combining operation between 2 values (+, - , * ...)
type Op a = a -> a -> a

-- select each element of some values together with the leftovers (easy)
selectEach :: S a -> [(a, S a)]
selectEach = go []
  where
    go _ [] = []
    go bf (x : xs) = (x, bf ++ xs) : go (x : bf) xs

-- given the possible combining operations and some values
-- combine each 2 values with each operation (no assumption is made on them)
-- use 'selectEach' (hard, use `concatMap` twice)
-- report every combined value together with 
-- the original list without the 2 combined value and with the combined one
-- (the list should not preserve any order)
-- i.e.
-- combineOnce [(+),(-)] [1,3,5] 
--     == [(4,[4,5]), (6,[6,3]), (8,[8,1]), (-2,[-2,5]), (-4,[-4,3]),(-2,[-2,1])]
combineOnce :: [Op a] -> S a -> [(a, S a)]
combineOnce fs = go []
  where
    go bf (x : xs) = concatMap (\op -> (f (op x) xs) ++ (go (x : bf) xs)) fs
    go _ [] = []
    f opx xs' = fmap (g opx) xs'
    g opx' x'' = (opx' x'', [opx' x''])



-- recurse combineOnce so that all possible ricombination is produced (easy)
combineAll :: [Op a] -> S a -> [a]
combineAll fs xs = concatMap (go []) (combineOnce fs xs)
  where
    go acc (x, []) = x:acc 
    go acc (x, ts) = concatMap (go (x:acc)) $ combineOnce fs ts
      

-- reification of the operations in structure to allow rendering
-- the value at the node is not really necessary, just a speedup
data A op a = L a | N op a (A op a) (A op a) deriving (Show)

-- value at the node
value :: A op a -> a
value (L x) = x
value (N _ x _ _) = x

-- produces all operations combinations over the given list
run :: [op] -> (op -> a -> a -> a) -> [a] -> [A op a]
run ops compute xs =
  combineAll
    [\x y -> N op (compute op (value x) (value y)) x y | op <- ops]
    $ L <$> xs

-- render a reification
render :: (Show b) => (op -> String) -> A op b -> IO ()
render symbol = putStrLn . report
  where
    clean (L x) = show x
    clean (N op _ l r) = "(" ++ clean l ++ symbol op ++ clean r ++ ")"

    report (L x) = show x
    report n@(N _ b _ _) = show b ++ " = " ++ (init . tail $ clean n)

-- application

data O = Sum | Prod | Diff deriving Show

valueO :: Num a => O -> a -> a -> a
valueO op x y = case op of
  Sum -> x + y
  Diff -> x - y
  Prod -> x * y

runO :: Num b => [b] -> [A O b]
runO = run [Sum, Diff, Prod] valueO

renderO :: O -> String
renderO Sum = "+"
renderO Prod = "*"
renderO Diff = "-"

test :: IO ()
test = mapM_ (render renderO) $ runO [7 :: Int, 13, 17]

{-

*Counting> test

20 = 7+13
37 = (7+13)+17
37 = 17+(7+13)
3 = (7+13)-17
-3 = 17-(7+13)
340 = (7+13)*17
340 = 17*(7+13)
20 = 13+7
37 = (13+7)+17
37 = 17+(13+7)
3 = (13+7)-17
-3 = 17-(13+7)
340 = (13+7)*17
340 = 17*(13+7)
-6 = 7-13
11 = (7-13)+17
11 = 17+(7-13)
-23 = (7-13)-17
23 = 17-(7-13)
-102 = (7-13)*17
-102 = 17*(7-13)
6 = 13-7
23 = (13-7)+17
23 = 17+(13-7)
-11 = (13-7)-17
11 = 17-(13-7)
102 = (13-7)*17
102 = 17*(13-7)
91 = 7*13
108 = (7*13)+17
108 = 17+(7*13)
74 = (7*13)-17
-74 = 17-(7*13)
1547 = (7*13)*17
1547 = 17*(7*13)
91 = 13*7
108 = (13*7)+17
108 = 17+(13*7)
74 = (13*7)-17
-74 = 17-(13*7)
1547 = (13*7)*17
1547 = 17*(13*7)
24 = 7+17
37 = (7+17)+13
37 = 13+(7+17)
11 = (7+17)-13
-11 = 13-(7+17)
312 = (7+17)*13
312 = 13*(7+17)
24 = 17+7
37 = (17+7)+13
37 = 13+(17+7)
11 = (17+7)-13
-11 = 13-(17+7)
312 = (17+7)*13
312 = 13*(17+7)
-10 = 7-17
3 = (7-17)+13
3 = 13+(7-17)
-23 = (7-17)-13
23 = 13-(7-17)
-130 = (7-17)*13
-130 = 13*(7-17)
10 = 17-7
23 = (17-7)+13
23 = 13+(17-7)
-3 = (17-7)-13
3 = 13-(17-7)
130 = (17-7)*13
130 = 13*(17-7)
119 = 7*17
132 = (7*17)+13
132 = 13+(7*17)
106 = (7*17)-13
-106 = 13-(7*17)
1547 = (7*17)*13
1547 = 13*(7*17)
119 = 17*7
132 = (17*7)+13
132 = 13+(17*7)
106 = (17*7)-13
-106 = 13-(17*7)
1547 = (17*7)*13
1547 = 13*(17*7)
30 = 13+17
37 = (13+17)+7
37 = 7+(13+17)
23 = (13+17)-7
-23 = 7-(13+17)
210 = (13+17)*7
210 = 7*(13+17)
30 = 17+13
37 = (17+13)+7
37 = 7+(17+13)
23 = (17+13)-7
-23 = 7-(17+13)
210 = (17+13)*7
210 = 7*(17+13)
-4 = 13-17
3 = (13-17)+7
3 = 7+(13-17)
-11 = (13-17)-7
11 = 7-(13-17)
-28 = (13-17)*7
-28 = 7*(13-17)
4 = 17-13
11 = (17-13)+7
11 = 7+(17-13)
-3 = (17-13)-7
3 = 7-(17-13)
28 = (17-13)*7
28 = 7*(17-13)
221 = 13*17
228 = (13*17)+7
228 = 7+(13*17)
214 = (13*17)-7
-214 = 7-(13*17)
1547 = (13*17)*7
1547 = 7*(13*17)
221 = 17*13
228 = (17*13)+7
228 = 7+(17*13)
214 = (17*13)-7
-214 = 7-(17*13)
1547 = (17*13)*7
1547 = 7*(17*13)

-}
