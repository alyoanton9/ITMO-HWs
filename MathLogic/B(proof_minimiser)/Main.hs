module Main where

import Grammar
import Lexer
import Parser
import Axioms
import Utils
import Output
import System.Environment
import System.IO
import System.Exit
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.Split

defExprMap = Map.empty
defInfoList = []
defExpr = Var ""
defMPMap = Map.empty

--read head and save hypos with prooving expression
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

readInput :: Int -> [Expression] -> Map.Map Expression Int -> [(Expression, Info)]
            -> Map.Map Expression Determ -> Expression
            -> IO (Int, Map.Map Expression Int, [(Expression, Info)], 
                    Map.Map Expression Determ, Expression)
readInput ind hypos eMap infoList mpMap prevExpr = do
    done <- isEOF
    if not done
    then do
        str <- getLine 
        let expr = parseExpr str
        let aInd = axIndex expr
        let hInd = hypoIndex expr hypos
        let mpMap' = editMPMap' expr eMap mpMap
        --base is A in (A, A->B => B) if B is MP
        let (mpMap'', base) = if (aInd == 0 && hInd == 0)
                                then editMPMap'' expr eMap mpMap'
                                else (mpMap', base)
        let mpInds = if (base /= defExpr)
                        then mpIndexes base expr eMap
                        else (0, 0)
        let (newExprMap, newInfoList, 
                        changed) = if aInd /= 0
                                    then addAx aInd ind
                                              expr eMap infoList
                                    else 
                                        if hInd /= 0
                                            then addHypo hInd ind 
                                                        expr eMap infoList
                                            else
                                                if mpInds /= (0, 0)
                                                    then addMP mpInds ind 
                                                            expr eMap infoList
                                                    else (Map.empty, [], True)
        {--putStrLn "mpMap'':"
        print mpMap''
        putStrLn "eMap:"
        print newExprMap--}
        if (null newInfoList)
            then do
                putStrLn "Proof is incorrect"
                exitSuccess 
            else 
                if (changed == False)
                    then do
                        tail <- readInput ind hypos eMap infoList mpMap'' expr
                        return tail
                    else do
                        tail <- readInput (ind + 1) hypos newExprMap
                            newInfoList mpMap'' expr
                        return tail        
    else 
        return (ind - 1, eMap, infoList, mpMap, prevExpr)


main :: IO ()
main = do
    (hypos, proving) <- readHead
    (lastInd, eMap, revInfoList, mpMap, lastExpr) <- readInput 1 hypos defExprMap defInfoList defMPMap defExpr
    let infoList = reverse revInfoList
    
    if lastExpr /= proving
        then do
            putStrLn "Proof is incorrect"
        else do
            putStrLn (printHead hypos proving)
            let used = tickUsed lastExpr eMap infoList
            let (revFiltEInfo, revFiltIInfo) = filterProof eMap infoList [] [] used
            let filtEInfo = reverse revFiltEInfo
            let filtIInfo = reverse revFiltIInfo
            let bijection = bijectiveNums filtIInfo
            proof <- printProof filtEInfo filtIInfo bijection    
            putStrLn proof
