{-# OPTIONS_GHC -Wall #-}
module Filiushkin01 where

-- Задача 1 -----------------------------------------
lengthMy :: [Int] -> Int
lengthMy xs = if null xs then 0 else 1 + lengthMy (tail xs)

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs && null ys then [] else (if null xs then 0 else head xs) + (if null ys then 0 else head ys) : listSum (if null xs then [] else tail xs) (if null ys then [] else tail ys)

-- Задача 3 -----------------------------------------
greatMy :: [Int] -> Int -> Int
greatMy xs v = if null xs then 0 else (if head xs > v then 1 else 0) + greatMy (tail xs) v

-- Задача 4 -----------------------------------------
elemMy    ::  Int -> [Int] -> Bool
elemMy x xs = not (null xs) && head xs == x || elemMy x (tail xs)
                     
-- Задача 5 -----------------------------------------
allMy :: [Bool] -> Bool
allMy xs = null xs || head xs && allMy (tail xs)

