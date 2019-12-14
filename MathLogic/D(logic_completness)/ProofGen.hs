module ProofGen(
    genHypoMaps,
    getAllProofs,
    reduceAll,
    toNormalProof,
    printHead,
    deductionTh
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Utils
import Grammar
import TT
import TTValues
import Elementary
import InfoMaker

--generate hypoMaps
--pos means positive
--May be to join everywhere instead of concatenating lists

genHypoMaps :: Bool -> Set.Set String -> Set.Set String
                -> [Map.Map String Bool]
genHypoMaps positive varSet minVarSet = hypoMap
    where   changeVarsList = Set.toList (Set.difference varSet minVarSet) --vars we can change (!)
            valuesTT = case (length changeVarsList) of
                            0 -> []
                            1 -> valuesTT1
                            2 -> valuesTT2
                            3 -> valuesTT3
            minVarSize = Set.size minVarSet
            minVarsValues = if positive
                                then Map.fromList (zip (Set.toList minVarSet) 
                                                        (replicate minVarSize True))
                                else Map.fromList (zip (Set.toList minVarSet) 
                                                        (replicate minVarSize False))
            hypoMap = if (minVarSize == Set.size varSet)
                        then [minVarsValues]
                        else map (\t -> Map.union minVarsValues 
                                                    (Map.fromList 
                                                    (zip changeVarsList t))) valuesTT

--generate proof from each hypoMap
genSubProof :: Expression -> Map.Map String Bool 
                -> (Bool, [(String, Info)])
genSubProof (Bin Impl a b) hypoMap = (implValue, implProof)
    where   (aValue, aProof) = genSubProof a hypoMap
            (bValue, bProof) = genSubProof b hypoMap
            implValue = not aValue || bValue
            implProof = aProof ++ bProof ++ 
                        (proveBin Impl (show a, aValue)
                                        (show b, bValue))

genSubProof (Bin Disj a b) hypoMap = (orValue, orProof)
    where   (aValue, aProof) = genSubProof a hypoMap
            (bValue, bProof) = genSubProof b hypoMap
            orValue = aValue || bValue
            orProof = aProof ++ bProof ++ 
                        (proveBin Disj (show a, aValue)
                                        (show b, bValue))

genSubProof (Bin Conj a b) hypoMap = (andValue, andProof)
    where   (aValue, aProof) = genSubProof a hypoMap
            (bValue, bProof) = genSubProof b hypoMap
            andValue = aValue && bValue
            andProof = aProof ++ bProof ++ 
                        (proveBin Conj (show a, aValue)
                                        (show b, bValue))

genSubProof (Not a) hypoMap = (notValue, notProof)
    where   (aValue, aProof) = genSubProof a hypoMap
            notValue = not aValue
            notProof = aProof ++ proveUn (show a, aValue)

genSubProof (Var var) hypoMap = (varValue, varProof)
    where   varValue = hypoMap Map.! var
            --varProof = proveUn (var, varValue)
            varProof = if varValue
                        then [(var, AxHypo)]
                        else [("!(" ++ var ++ ")", AxHypo)]


deductionTh :: String -> [[(String, Info)]] -> [(String, Info)]
                -> [[(String, Info)]]
--seems like it works
deductionTh aStr newProof ((s, i) : si) = deductionTh aStr 
                                                (genFromS : newProof) si
    where   genFromS = if (parseExpr aStr == parseExpr s)
                        then reverse $! replaceInTempl1 'A' aStr [] $! self
                        else
                            case i of
                                AxHypo -> reverse $! replaceInTempl2 'A' 'B' 
                                                            s aStr [] $! axHypo
                                MP mpS -> reverse $! replaceInTempl3 'A' 'B' 'C'
                                                            aStr mpS s[] $! mp
deductionTh _ newProof [] = newProof


getAllProofs :: Expression -> [Map.Map String Bool] 
                -> [[(String, Info)]]
getAllProofs expr hypoMaps = map (\hm -> snd (genSubProof expr hm))
                                    hypoMaps


--           parameter  proving     
mergeProofs :: String -> String -> [(String, Info)] -> [(String, Info)]
                -> [(String, Info)]
mergeProofs param proving proof1 proof2 = proof1 ++ proof2 ++ merged
    where merged = replaceInTempl2 'B' 'C' param proving [] $! thirdExcl


reduce :: String -> [(String, Info)] -> [(String, Info)] 
            -> [(String, Info)]
reduce deductVar proof1 proof2 = reducedProof
    where   deductProof1' = deductionTh ( "!("  ++ deductVar ++  ")" ) [] $! proof1 
            deductProof2' = deductionTh deductVar [] $! proof2 
            deductProof1 = reverse $! join [] deductProof1' -- will be list of lists
            deductProof2 = reverse $! join [] deductProof2'
            reducedProof = ((mergeProofs deductVar (fst . last $! proof1)) 
                                        $! deductProof1) $! deductProof2

reduceByPairs :: String -> [[(String, Info)]] -> [[(String, Info)]]
                    -> [[(String, Info)]]
reduceByPairs deductVar newProofs 
                (proof1 : (proof2 : proofs))  = reduceByPairs deductVar 
                                                                (reduced : newProofs) $! proofs
    where reduced = ((reduce deductVar) $! proof1) $! proof2
reduceByPairs _ newProofs [] = newProofs

reduceAll :: Set.Set String -> [[(String, Info)]] -> [[(String, Info)]]
reduceAll changeVars proofs = if (null changeVars)
                                then proofs
                                else reduceAll changeVars' $! reverse $! reduceByPairs 
                                                        deductVar [] $! proofs
    where   deductVar = last . Set.toList $! changeVars
            changeVars' = Set.difference changeVars $! Set.fromList [deductVar]


separateProof :: [(String, Info)] -> [String]
separateProof proof = map (\p -> fst p) proof

toNormalProof :: [(String, Info)] -> String
toNormalProof proof = join "\n" (separateProof $! proof)

printHead :: Bool -> [String] -> String -> IO ()
printHead pos hypos proving = putStrLn ((intercalate ", " hypos') 
                                        ++ " |- " ++ proving)
    where hypos' = if pos 
                    then hypos
                    else (map (\h -> "!(" ++ h ++ ")") hypos)

