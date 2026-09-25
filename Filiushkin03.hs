{-# OPTIONS_GHC -Wall #-}
module Filiushkin03 where

-- Задача 1 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^i | i <- [1..n]]

-- Задача 2 ---------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Задача 3 -----------------------------------------
myButLast :: [Int] -> Int
myButLast [] = 0
myButLast [_] = 0
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Задача 4 -----------------------------------------
all35 :: Int -> [Int]
all35 n = [x | x <- [1..n - 1], mod x 3 == 0, mod x 5 == 0]

-- Задача 5 -----------------------------------------
phi :: Int -> Int
phi n = length [x | x <- [1..n - 1], gcd x n == 1]

-- Задача 6 -----------------------------------------
maxSuf :: [Int] -> Int
maxSuf xs = (maximum . map sum . scanr (:) []) xs

-- Задача 7 -----------------------------------------
lastTail :: String -> String
lastTail xs = (maximum . scanr (:) []) xs

-- Задача 8 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = [i | i <- [0..length ys - length xs], take (length xs) (drop i ys) == xs]
