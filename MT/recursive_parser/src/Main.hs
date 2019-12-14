module Main where

import Grammar
import ParserImpl as P
import Tokenizer
import Visual

main :: IO()
main = do
    putStrLn $ "Enter your expression"
    input <- getLine
    putStrLn $ "Enter the name of the file with parse tree"
    filename <- getLine
    let tokens = tokenize input
    putStrLn $ "Tokens are: "
    putStrLn . show $ tokens
    let expr = P.parse tokens
    putStrLn . show . evaluate $ expr
    let (vs, es) = makeExprGraph expr
    makeDotGraph (vs, es) filename
    