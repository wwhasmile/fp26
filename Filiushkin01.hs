{-# OPTIONS_GHC -Wall #-}
module Filiushkin01 where

-- Задача 1 -----------------------------------------
lengthMy :: [Int] -> Int
lengthMy [] = 0
lengthMy xs = 1 + lengthMy (tail xs)

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum [] [] = []
listSum [] (y:ys) = y : listSum [] ys
listSum (x:xs) [] = x : listSum xs []
listSum (x:xs) (y:ys) = x + y : listSum xs ys

-- Задача 3 -----------------------------------------
greatMy :: [Int] -> Int -> Int
greatMy [] _ = 0
greatMy (x:xs) v = (if x > v then 1 else 0) + greatMy xs v

-- Задача 4 -----------------------------------------
elemMy :: Int -> [Int] -> Bool
elemMy _ [] = False
elemMy x (v:xs) = v == x || elemMy x xs
                     
-- Задача 5 -----------------------------------------
allMy :: [Bool] -> Bool
allMy [] = True
allMy (x:xs) = x && allMy xs
