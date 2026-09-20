{-# OPTIONS_GHC -Wall #-}
module Filiushkin02 where

-- Задача 1 -----------------------------------------
sumFr :: [Int] -> Int
sumFr xs = foldr (+) 0 xs

-- Задача 2 -----------------------------------------
andFr :: [Bool] -> Bool
andFr xs  = foldr (&&) True xs

-- Задача 3 -----------------------------------------
maximumFl :: [Int] ->  Int
maximumFl xs = foldl1 (\x y -> if x > y then x else y)  xs

-- Задача 4 -----------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int 
cntGood ps v = length $ filter ($ v) ps

-- Задача 5 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map (reverse) xss)
