--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem, zip )

import Data.List (delete)

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

-- a -> (b -> c), higher-order 
-- (a -> b) -> Int, higher-order 
-- (Int, Bool) -> Char, not higher-order 
-- a -> a, higher-order 

-- id :: a -> a 
-- not :: Bool -> Bool
-- (Bool -> Bool) -> (Bool -> Bool)


elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\y r -> x==y || r) False

maximum :: Ord a => [a] -> a
maximum = foldr1 max 

any :: (a -> Bool) -> [a] -> Bool
any p [] = False 
any p (x:xs) = p x || any p xs 

all :: (a -> Bool) -> [a] -> Bool
all p [] = True 
all p (x:xs) = p x && all p xs

flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) 
    | p x       = x : takeWhile p xs
    | otherwise = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = zipWith (,) xs ys

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f []     = []
groupBy f (x:xs) = (x:ys) : groupBy f zs
    -- where ys = takeWhile (f x) xs
    --       zs = dropWhile (f x) xs
    where (ys, zs) = span (f x) xs

--    permutations "abc"
-- => ["abc", "acb", "bac", "bca", "cab", "cba"]

--    permutations [1,2,3]
-- => [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, 
                          ys <- permutations (delete x xs)]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = go xs []
    where go []     _  = []
     --   go (y:ys) zs = [y:as | as <- permutations' (zs++ys)] ++ go ys (y:zs)
          go (y:ys) zs = map (y:) (permutations' (zs++ys)) ++ go ys (y:zs)

--    permutations' [1,2,3]
-- => go [1,2,3] [] 
-- => [1:as | as <- permutations' [2,3]] ++ go [2,3] (1:[])
-- => [1:as | as <- go [2,3] []] ++ go [2,3] (1:[])
-- => [1:as | as <- [2:as | as <- permutations' [3]] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [2:as | as <- go [3] []] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [2:as | as <- [3:as | as <- permutations' []] ++ go [] (3:[])] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [2:as | as <- [[3]] ++ go [] (3:[])] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [2:as | as <- [[3]]] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3]] ++ go [3] (2:[])] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3]] ++ [3:as | as <- permutations' ([2]++[])] ++ go [] (3:[2])] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3]] ++ [3:as | as <- permutations' [2]]] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3]] ++ [3:as | as <- [[2]]]] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3]] ++ [[3,2]]] ++ go [2,3] (1:[])
-- => [1:as | as <- [[2,3],[3,2]]] ++ go [2,3] (1:[])
-- => [[1,2,3], [1,3,2]] ++ go [2,3] (1:[])

--------------------------------------------------------------------------------
