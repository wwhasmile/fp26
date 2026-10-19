{-# OPTIONS_GHC -Wall #-}
module Filiushkin06 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Helpers
prodFromControl :: Control -> Char -> Char -> Maybe Int
prodFromControl ct n t = lookup (n,t) ct
prodById :: Grammar -> Int -> Maybe Production
prodById gr i
  | i >= length gr = Nothing
  | otherwise = Just (gr !! i)

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String
addOne [] c = [c]
addOne (s:st) c
  | s == c = (s:st)
  | s > c = c:s:st
  | otherwise = (s:(addOne st c))

addAll :: String -> String -> String 
addAll st wd = foldl' addOne st wd 

addWithout :: String -> String -> String 
addWithout st wd = foldl' (\b a -> if a == '$' then b else addOne b a) st wd

inter :: String -> String -> String 
inter st1 st2 = addAll "" $ filter (\a -> any (\b -> a == b) st1 && any (\b -> a == b) st2) $ st1 ++ st2

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict ptr n = maybe "" snd (find (\a -> fst a == n) ptr)

upPredict :: Predict -> Char -> String -> Predict
upPredict [] n st = [(n,st)] 
upPredict ((k,v):pr) n st
  | k == n = (n,st) : pr
  | k > n = (n,st) : (k,v) : pr
  | otherwise = (k,v) : upPredict pr n st

-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl wd =
  case until untilCond (step gr ctl) (wd, "S", Just []) of
    (_, _, Just r) -> Just (reverse r)
    _ -> Nothing
  where
    untilCond (_, stack, result) = case result of
      Just _ -> null stack
      Nothing -> True

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl ((ii:inputs), (ss:stack), result)
  | isUpper ss = step gr ctl ((ii:inputs), (p ++ stack), Just $ i : (maybe [] (\r -> r) result))
  | ii == ss = (inputs, stack, result)
  | otherwise = (inputs, stack, Nothing)
  where
    i = maybe 0 (\r -> r) (prodFromControl ctl ss ii)
    (_,p) = maybe ('S', "") (\r -> r) (prodById gr i)
step _ _ (input, stack, _) = (input, stack, Nothing)

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ "" = "$"
first pFst (s:st)
  | elem '$' cFst = addWithout (first pFst st) cFst
  | otherwise = cFst
  where
    cFst = case lookup s pFst of
      Just p -> p
      Nothing -> [s]

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl = undefined

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 = undefined

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar = undefined

testFst :: [String] -> Bool
testFst = undefined

testFollow :: String -> [String] -> Bool
testFollow = undefined

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
buildFst = undefined

evalFst :: Grammar -> Predict -> Predict 
evalFst = undefined

extandFst :: Predict -> Production -> Predict 
extandFst = undefined

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt = undefined

nontermTails :: Grammar -> [(Char,String)] 
nontermTails = undefined

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

