module Main where

import Grammar
import Lexer
import Parser
import Data.List
import System.IO
import System.Exit

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

getInput :: [String] -> IO [String]
getInput input = do
    done <- isEOF
    if done
        then do
            return input
        else do
            line <- getLine
            getInput $ line : input

main :: IO()
main = do
    reversedInput <- getInput []
    let input = join " " (reverse reversedInput)
    let expr = parse (alexScanTokens input)
    putStrLn . show $ expr