module Main where

import Grammar
import Lexer
import Parser

main :: IO()
main = do
    input <- getLine
    let expr = parse (alexScanTokens input)
    putStrLn . show $ expr
