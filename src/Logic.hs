module Logic (
                Logic(..),
                parse,
                eval
            )where 

import Data.Char
import Data.Maybe
import Util

--TODO : add implication and biconditonal

data Logic = Var Char
            |And Logic Logic
            |Or Logic Logic
            |Not Logic
            deriving Show

type VariableValues = [(Char, Bool)]

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
