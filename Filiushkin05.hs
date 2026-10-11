{-# OPTIONS_GHC -Wall #-}
module Filiushkin05 where

import Data.List

type Grammar = [Production]
type Production = (Char,String)
-- Граматика - список продукцій.
--   нетермінал першої - початковий

-- Лівосторонній вивід - послідовність слів  [String] з іншого боку
--     послідовність номерів правил, які при цьому застосовувались [Int]
type DerivationS = [String] 
type DerivationR = [Int] 

-- Helpers
isNonTerminal, isTerminal :: Char -> Bool
isNonTerminal c = c >= 'A' && c <= 'Z'
isTerminal c = not (isNonTerminal c) && c /= '$'

isLeftR :: String -> Production -> Bool
isLeftR _ (_,[]) = False
isLeftR s (lhc,(f:rhc)) = isNonTerminal f && (lhc == f || elem f s || isLeftR (f : s) (lhc,(f:rhc)))

-- Задача 1 -----------------------------------------
isGrammar :: Grammar -> Bool
isGrammar gr =
    not (null gr) &&
    all isNonTerminal lhcs &&
    allUnique gr &&
    all isValidRhc rhcs
  where
    lhcs = map fst gr
    rhcs = map snd gr

    allUnique xs = length xs == (length (nub xs))
    isValidRhc rhc = all (/= '$') rhc

-- Задача 2.a ---------------------------------------
allTerm :: Grammar -> String
allTerm gr =
    (nub . filter isTerminal . concatMap snd) gr
 
-- Задача 2.b ---------------------------------------
allNotT :: Grammar -> String
allNotT gr =
    (nub . filter isNonTerminal . concatMap (\(lhc,rhc) -> rhc ++ [lhc])) gr

-- Задача 2.c ---------------------------------------
newMinN :: Grammar -> Char
newMinN gr = newMinNImpl 'A' (sort (allNotT gr))
  where
    newMinNImpl r [] = r
    newMinNImpl r (c:nt) = newMinNImpl (if c == r then succ r else r) nt

-- Задача 3.a -----------------------------------------
buildGen :: Grammar -> String
buildGen gr = buildGenImpl "" gr
  where
    buildGenImpl r [] = r
    buildGenImpl r ((lhc,rhc):prs)
      | all (\c -> notElem lhc r && (elem c r || isTerminal c)) rhc = buildGenImpl (lhc : r) gr
      | otherwise = buildGenImpl r prs

-- Задача 3.b -----------------------------------------
buildAcc :: Grammar -> String
buildAcc gr = buildAccImpl "S"
  where
    buildAccStep r [] = r
    buildAccStep r ((lhc,rhc):prs)
      | elem lhc r = buildAccStep ([x | x <- rhc, notElem x r, isNonTerminal x] ++ r) prs
      | otherwise = buildAccStep r prs

    buildAccImpl r
        | next == r = next
        | otherwise = buildAccImpl next
      where
        next = buildAccStep r gr
        
-- Задача 3.c -----------------------------------------
reduce :: Grammar -> Grammar
reduce gr = [(lhc,rhc) | (lhc,rhc) <- gr, let frhc = filter isNonTerminal rhc, elem lhc gen, elem lhc acc, length (frhc \\ gen) == 0, length (frhc \\ acc) == 0]
  where
    gen = buildGen gr
    acc = buildAcc gr

-- Задача 4.a -----------------------------------------
findLeftR :: Grammar -> String
findLeftR gr = nub [lhc | (lhc,rhc) <- gr, isLeftR [lhc] (lhc,rhc)]

-- Задача 4.b -----------------------------------------
deleteLeftR :: Grammar -> Char -> Grammar
deleteLeftR = undefined

-- Задача 5.a -----------------------------------------
isFact :: Grammar -> Char -> Bool
isFact = undefined

-- Задача 5.b -----------------------------------------
deleteFact :: Char -> String -> Grammar -> Grammar
deleteFact = undefined

-- Задача 6.a -----------------------------------------
isLeftDerivationS :: Grammar -> DerivationS -> Bool
isLeftDerivationS = undefined

-- Задача 6.b -----------------------------------------
isLeftDerivationR :: Grammar -> DerivationR -> Bool
isLeftDerivationR = undefined

-- Задача 7 -----------------------------------------
fromLeftR :: Grammar -> DerivationR -> DerivationS
fromLeftR = undefined

--------------------------------------------------------
--  тестові дані 
gr0, gr1, gr1e, gr2 :: Grammar   
gr0 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba")]
gr1 = [ ('S',"aSa"), ('S',"bSd"), ('S',"c"), ('S',"aSb"), ('D',"aC") 
      , ('A',"cBd"), ('A',"aAd"),('B',"dAf"),('C',"cS"), ('C',"a")]
gr1e = [('S',"aAS"), ('S',"a"),('a',"SbA"),('A',"ba"),('S',"")]
gr2 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   

gr0S, gr0Se :: [String]
gr0S = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabbaa"]
gr0Se = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabba"]

gr0R :: DerivationR
gr0R = [0, 2, 1, 3, 1]



