module Logic (
                Logic(..),
                parse,
                eval
            )where 

import Data.Char (toUpper)
import Data.Maybe(Maybe(..), fromMaybe)
import Util (infixToPrefix, Infix)

data Logic = Var Char
            |And Logic Logic
            |Or Logic Logic
            |Not Logic
            deriving Show
type VariableValues = [(Char, Bool)]

--TODO : add implication and biconditonal

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
            parseThis ('>':xs) = let (x1,xs1) = parseThis xs
                                     (x2,xs2) = parseThis xs1
                                 in  (impl x1 x2, xs2)
            parseThis ('<':xs) = let (x1,xs1) = parseThis xs
                                     (x2,xs2) = parseThis xs1
                                 in  (impl x2 x1, xs2)
            parseThis ('#':xs) = let (x1,xs1) = parseThis xs
                                     (x2,xs2) = parseThis xs1
                                 in (And (impl x1 x2) (impl x2 x1), xs2)
            parseThis (x:xs) = (Var (toUpper x), xs)
            impl :: Logic -> Logic -> Logic
            impl a b = Or (Not a) (b)

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
