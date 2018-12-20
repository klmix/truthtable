module Wahrheitstabelle 
        (
            createTable,
            printTable
        )where 

import Data.List (sort,intersperse)
import Logic (Logic(..),eval)

--Erstellt alle Kombination fLength langer listen von Bool-werten, 
createCombinations :: Int -> [[Bool]]
createCombinations fLength = let begining = take fLength (repeat False) --Urprungsliste besteht nur aus FALSE
                             in  begining : cCom begining
    where 
        cCom :: [Bool] -> [[Bool]]
        cCom ls = let list = currentLineCombination ls 
                  in if and list --Abbruch falls alle Werte der Liste TRUE
                     then [list] 
                     else list : cCom list
        
        currentLineCombination :: [Bool] -> [Bool]
        currentLineCombination ls
            |and ls = ls 
            |not $ last ls = init ls ++ [True] -- Ändert die aktuelle Position zu TRUE
            -- "" zu FALSE und übertragt auf nächstes Element
            |otherwise = currentLineCombination (init ls) ++ [False] 

--Erstellt eine Liste aus allen Variablen Namen
createNameList :: Logic -> String
createNameList lgc = sort $ getNames lgc
        where
            getNames :: Logic -> String
            getNames (Not a) = getNames a
            getNames (And a b) = (getNames a) ++ (getNames b)
            getNames (Or a b)  = (getNames a) ++ (getNames b)
            getNames (Var a) = [a]

assignValue :: Logic -> [[(Char,Bool)]]
assignValue lgc = let varTable = createNameList lgc 
                  in map (zip varTable) (createCombinations (length varTable))

createTable :: Logic -> [String]
createTable lgc = let aV = assignValue lgc
            in map createLine aV
    where createLine val = (foldl (\x y -> x ++ (show.snd) y ++ " & ") "") val ++ show (eval lgc val) ++ "\\\\"

printTable :: Logic -> String -> IO()
printTable lgc orig = putStrLn (unlines $ headLine ++ createTable lgc)
    where headLine = [(intersperse '\t' (createNameList lgc))++('\t':orig)]
