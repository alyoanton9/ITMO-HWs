module Main where

import ProofGen
import Elementary
import Utils
import TT
import Grammar

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO()
main = do
    eStr <- getLine 
    let expr = parseExpr eStr
    let tt = makeTT expr
    --let varSet = getVars expr Set.empty
    if (isUnProovable tt)
        then putStrLn ":("
        else do
            let positiveSet = isPositive tt
            --print positiveSet
            let expr' = if positiveSet 
                            then expr
                            else (Not expr)
            let varSet = getVars expr' Set.empty
            let minVarSet = if positiveSet
                                then findMinSet (Set.toList varSet) tt
                                else findMinSet (Set.toList varSet) $! makeTT (negVars expr')
            --print minVarSet

            let changeVarSet = Set.difference varSet minVarSet
            let hypoMaps = genHypoMaps positiveSet varSet minVarSet
            
            let allProofs = getAllProofs expr' hypoMaps
            let reduced = reduceAll changeVarSet $! allProofs
            
            printHead positiveSet (Set.toList minVarSet) (show expr')
            putStrLn (toNormalProof $! reduced !! 0)
            --print (reduced !! 0)
            return ()

            {--[("!(A)",AxHypo),("!(B)",AxHypo),("((A)&((!B)))->(A)",AxHypo),("!(A)->(((A)&((!B)))->!(A))",AxHypo),
            ("((A)&((!B)))->!(A)",MP "!(A)"),("(((A)&((!B)))->(A))->((((A)&((!B)))->!(A))->!((A)&((!B))))",AxHypo),
            ("(((A)&((!B)))->!(A))->!((A)&((!B)))",MP "((A & (!B)) -> A)"),("!((A)&((!B)))",MP "((A & (!B)) -> !(A))"),
            ("(!(A & !B))",AxHypo),("(!(A & !B))->(!!!(!(A & !B))->(!(A & !B)))",AxHypo),("!!!(!(A & !B))->(!(A & !B))",MP "!(A & !B)"),
            ("!!!(!(A & !B))->!(!(A & !B))",AxHypo),("(!!!(!(A & !B))->(!(A & !B)))->((!!!(!(A & !B))->!(!(A & !B)))->!!!!(!(A & !B)))",AxHypo),("(!!!(!(A & !B))->!(!(A & !B)))->!!!!(!(A & !B))",MP "(!!!(!(A & !B)) -> !(A & !B))"),("!!!!(!(A & !B))",MP "(!!!(!(A & !B)) -> !(!(A & !B)))"),("!!!!(!(A & !B))->!!(!(A & !B))",AxHypo),
            ("!!(!(A & !B))",MP "!!!!(!(A & !B))")]--}