module Main where

import Grammar
import Lexer
import Parser
import Axioms
import Utils
import System.IO
import System.Exit
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.List.Split

readHead :: IO ([Expression], Expression)
readHead = do
    head <- getLine
    let splitted = splitOn "|-" head
    let left = splitted !! 0
    let right = splitted !! 1
    let proving = parseExpr right
    case left of
        "" -> return ([], proving)
        _  -> let hypos = map (\str -> parseExpr str) 
                                (splitOn "," left)
              in do
                return (hypos, proving)

readAndDeduce :: [Expression] -> Set.Set Expression -> Map.Map Expression [Expression]
                -> IO ()
readAndDeduce hypos exSet mpMap = do
    done <- isEOF
    if done
        then do exitSuccess
        else do
            line <- getLine
            let expr = parseExpr line
            let eType = detectType expr hypos exSet mpMap
            deduceInIntuit expr hypos exSet mpMap
            let mpMap' = editMPMap expr mpMap
            let exSet' = Set.insert expr exSet
            readAndDeduce hypos exSet' mpMap'

main :: IO ()
main = do
    (hypos, proving) <- readHead
    putStrLn (printHead hypos proving)
    readAndDeduce hypos Set.empty Map.empty
    exitSuccess 