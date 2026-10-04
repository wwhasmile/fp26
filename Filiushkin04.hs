{-# OPTIONS_GHC -Wall #-}
module Filiushkin04 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int  deriving (Show, Eq)
type Program = [Command]
type ConfigС = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (b:bs) (x:xs) = b == x && isPrefix bs xs

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (a,b,_) i w 
  | not ip = w
  | otherwise = take i w ++ b ++ drop (i + length a) w
  where ip = isPrefix a $ drop i w

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w (a,b,c) = [((a,b,c), i) | i <- [0..length w], isPrefix a (drop i w)]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = foldl (\a b -> a ++ findPosition w b) [] algo

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (_,st,word) =
  case findAll algo word of
    [] -> (False, st, word)
    (((a,b,e),p):_) -> (not e, st + 1, substitute (a,b,e) p word)

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = case until (\(bt,st,_) -> st == m || not bt) (stepA algo) (True,0,word) of
  (True,_,_) -> Nothing
  (False,_,w) -> Just w

-- Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg pr =
  maximum $ map getN pr
  where
    getN c = case c of
      Z n -> n
      S n -> n
      T _ n -> n
      J _ n _ -> n

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir = ir ++ (take (maximReg pr - (length ir)) $ repeat 0)

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v = take r reg ++ [v] ++ drop (r + 1) reg

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigС -> ConfigС
stepC pr (nm,st,rg) =
  case drop (nm - 1) pr of
    [] -> (nm,st,rg)
    (cmd:_) -> case cmd of
      Z n -> (nm + 1, st + 1, upd rg (n - 1) 0)
      S n -> (nm + 1, st + 1, upd rg (n - 1) (rg !! (n - 1) + 1))
      T m n -> (nm + 1, st + 1, upd rg (n - 1) (rg !! (m - 1)))
      J m n q -> (if rg !! (n - 1) == rg !! (m - 1) then q else nm + 1, st + 1, rg)
    
-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir =
  let
    prl = length pr
    (nm,_,rg) = until (\(lnm,st,_) -> st == mx || lnm > prl) (stepC pr) (1, 0, ini pr ir)
  in case (Prelude.reverse rg) of
    (r:_) | nm > prl -> Just r
    _ -> Nothing
    
---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
