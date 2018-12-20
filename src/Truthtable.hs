module Truthtable
        (
            createTable,
            printTable
        )where 

import Data.List (sort,intersperse)
import Logic (Logic(..),eval)

{-Creates every combination of Lists with fLength of Booleans.
  Original List consists only of FALSE-}
createCombinations :: Int -> [[Bool]]
createCombinations fLength = let begining = take fLength (repeat False)
                             in  begining : cCom begining
    where 
        cCom :: [Bool] -> [[Bool]]
        cCom ls = let list = currentLineCombination ls 
                  in if and list --Cancels if every Listvalue ist True
                     then [list] 
                     else list : cCom list
      
        currentLineCombination :: [Bool] -> [Bool]
        currentLineCombination ls
            |and ls = ls 
            |not $ last ls = init ls ++ [True] 
            |otherwise = currentLineCombination (init ls) ++ [False] 

--Creates List consisting of every var name in order
createNameList :: Logic -> String
createNameList lgc = sort $ getNames lgc
        where
            getNames :: Logic -> String
            getNames (Not a) = getNames a
            getNames (And a b) = (getNames a) ++ (getNames b)
            getNames (Or a b)  = (getNames a) ++ (getNames b)
            getNames (Var a) = [a]

--Creates new List Consisteng of of every possible var-assignment
assignValue :: Logic -> [[(Char,Bool)]]
assignValue lgc = let varTable = createNameList lgc 
                  in map (zip varTable) (createCombinations (length varTable))

--Creates list of every row of the truthtable
createTable :: Logic -> [String]
createTable lgc = let aV = assignValue lgc
            in map createLine aV
    where createLine val = (foldl (\x y -> x ++ (show.snd) y ++ " & ") "") val ++ show (eval lgc val) ++ "\\\\"

--Prints the Table
printTable :: Logic -> String -> IO()
printTable lgc orig = putStrLn (unlines $ headLine ++ createTable lgc)
    where headLine = [(intersperse '\t' (createNameList lgc))++('\t':orig)]
