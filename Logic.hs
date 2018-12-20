module Logic (
                Logic(..),
                parse,
                eval
            )where 

import Data.Char
import Data.Maybe

data Logic = Var Char
            |And Logic Logic
            |Or Logic Logic
            |Not Logic
            deriving Show

type Infix = String
type VariableValues = [(Char, Bool)]

infixToPrefix :: String -> Infix
infixToPrefix prefix = transform (prefix, prefix)
    where
        transform :: (String, String) -> Infix
        transform (ls@(x:xs), orig)
            |x == '&' || x == '|' = transform (xs ,back x (length ls) orig)
            -- Abbruch bei länge des Restwortes von 1, da Und und Oder nur an vorletzter Stelle sein können.
            |length ls == 1 = orig 
            |otherwise = transform (xs , orig)
        --verschiebt aktuellen character nach hinten.
        back :: Char -> Int -> String -> String
        back ch l wort = let (old, new) = splitAt ((length wort) - l) (wort)
                             (swap,index) = (getS old 0)
                         in (take  (index-1) old ) ++ [ch] ++ (drop (index) old) ++ [swap] ++ (tail new)
        --findet den zu vertauschenden alten Character.
        getS :: String -> Int -> (Char, Int)
        getS str cnt
            |str == "" = error "Falsche länge"
            |last str == '(' = getS (init str) (cnt - 1)
            |last str == ')' = getS (init str) (cnt + 1)
            |cnt > 0         = getS (init str) cnt
            |otherwise       = (last str, length str)

parse :: Infix -> Logic
parse  = fst . parseThis . (filter (\x -> x /= '(' && x /= ')') ) . infixToPrefix
        where 
            parseThis ('&':xs) = let (x1,xs1) = parseThis xs
                                     (x2,xs2) = parseThis xs1
                                 in  (And x1 x2, xs2)
            parseThis ('|':xs) = let (x1,xs1) = parseThis xs
                                     (x2,xs2) = parseThis xs1
                                 in  (Or x1 x2, xs2)
            parseThis ('-':xs) = let (x1,xs1) = parseThis xs
                                 in  (Not x1, xs1)
            parseThis (x:xs) = (Var (toUpper x), xs)

eval:: Logic -> VariableValues -> Bool
eval x val = evalThis x
    where 
        evalThis :: Logic -> Bool
        evalThis (And a b) = (evalThis a) && (evalThis b)
        evalThis (Or a b) = (evalThis a) || (evalThis b)
        evalThis (Not a) = not $ evalThis a
        evalThis (Var a) = varVal a
        
        varVal :: Char -> Bool
        varVal name = fromMaybe False (lookup name val)
