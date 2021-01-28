--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , permutations, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem x (y:ys) = not (null (filter (== x) ys))

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\u v -> u == x || v) False

maximum :: Ord a => [a] -> a
maximum = foldr1 max

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\u v -> f u || v) False 

elem'' :: Eq a => a -> [a] -> Bool
elem'' x = any (== x)

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\u v -> f u && v) True

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f (x:xs) = takeWhile (f x) (x:xs) : groupBy f (dropWhile (f x) xs)

selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y : ps | (y,ys) <- selections xs, ps <- permutations ys]

--------------------------------------------------------------------------------
