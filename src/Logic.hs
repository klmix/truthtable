module Logic (
                Logic(..),
                parse,
                eval
            )where 

import Data.Char
import Data.Maybe

--TODO : add implication and biconditonal
data Logic = Var Char
            |And Logic Logic
            |Or Logic Logic
            |Not Logic
            deriving Show

type Infix = String
type VariableValues = [(Char, Bool)]

-- TODO: Very likely the Reason for bugs, change and rewrite in a new module "util"
infixToPrefix :: String -> Infix
infixToPrefix prefix = transform (prefix, prefix)
    where
        transform :: (String, String) -> Infix
        transform (ls@(x:xs), orig)
            |x == '&' || x == '|' = transform (xs ,back x (length ls) orig)
            -- Cancels at length 1 because last symbol can't be Operator
            |length ls == 1 = orig 
            |otherwise = transform (xs , orig)
        --moves current char back.
        back :: Char -> Int -> String -> String
        back ch l wort = let (old, new) = splitAt ((length wort) - l) (wort)
                             (swap,index) = (getS old 0)
                         in (take  (index-1) old ) ++ [ch] ++ (drop (index) old) ++ [swap] ++ (tail new)
        --finds correct place to swap.
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
--TODO should be -> Maybe Bool 
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
