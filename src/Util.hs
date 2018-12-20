module Util where

type Infix = String
type Prefix = String

infixToPrefix :: Infix -> Prefix
infixToPrefix inf = transform inf inf

transform :: Infix -> String -> Prefix
transform orig inf@(x:xs)
        |length inf <= 1 = orig
        |x == '|' || x == '&' = uncurry transform $ mvBack orig x xs
        |otherwise = transform orig xs

mvBack :: String -> Char -> String -> (String, String)
mvBack orig ch rest  = let (left, right) = splitAt (length orig - (length rest)) orig
                           leftLeft = mvTo ch (init left) 0
                           rightLeft = (drop ((length leftLeft)) (init left))
                           newLeft = leftLeft ++ [ch] ++ rightLeft
                        in (newLeft ++ right, rest)
mvTo :: Char -> String -> Int -> String
mvTo ch word brackets
    |word == "" = error "input failure: probably missing bracket"
    |last word == '(' && brackets >0 = mvTo ch (init word) (brackets - 1)
    |last word == ')' = mvTo ch (init word) (brackets + 1)
    |(safeLast $ init word) == (Just '-')  = mvTo ch (init word) brackets
    |otherwise = (init word)

safeLast :: String -> Maybe Char
safeLast str 
    |str == "" = Nothing
    |otherwise = Just $ last str

safeStrInit :: String -> String
safeStrInit str 
    |str == "" = ""
    |otherwise = init str

