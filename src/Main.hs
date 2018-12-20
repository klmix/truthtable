module Main where 

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Logic (parse)
import Truthtable(printTable)

main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Please enter Formula"
    formula <- getLine
    printTable (parse formula) formula
