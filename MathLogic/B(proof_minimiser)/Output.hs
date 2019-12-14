module Output(
    printHead,
    printProof,
    printProof'
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Exit
import Data.Maybe
import Grammar
import Utils

printHead :: [Expression] -> Expression -> String
printHead hypos proving = head
    where   hyposStr = intercalate ", " (map show hypos)
            provingStr = show proving
            head = hyposStr ++ " |- " ++ provingStr

proofStr' :: Int -> (Expression, Info) -> String
proofStr' index (expr, AxInfo aInd) = "[" ++ intercalate ". " [show index, 
                                                "Ax", "sch", show aInd] ++ "] "
                                            ++ show expr
proofStr' index (expr, HypoInfo hInd) = "[" ++ show index ++ ". Hypothesis " 
                                            ++ show hInd ++ "] " ++ show expr
proofStr' index (expr, MPInfo (i1, i2)) = "[" ++ show index ++ ". M.P. " 
                                            ++ show i1 ++ ", " ++ show i2 ++ "] "
                                            ++ show expr


proofStr :: Int -> (Expression, Info) -> Map.Map Int Int -> String
proofStr oldIndex (expr, MPInfo (i1, i2)) biMap = proofStr' realInd (expr, 
                                                MPInfo (realI1, realI2))
    where   realInd = fromMaybe 0 (Map.lookup oldIndex biMap)
            realI1 = fromMaybe 0 (Map.lookup i1 biMap)
            realI2 = fromMaybe 0 (Map.lookup i2 biMap)
proofStr oldIndex (expr, info) biMap = proofStr' realInd (expr, info)
    where   realInd = fromMaybe 0 (Map.lookup oldIndex biMap)


printProof :: [(Expression, Info)] -> [(Int, Info)] -> Map.Map Int Int -> IO String
printProof ((expr, info) : xsE) ((oldInd, _) : xsI) biMap = do
    let strToPrint = proofStr oldInd (expr, info) biMap
    putStrLn strToPrint
    tailProof <- printProof xsE xsI biMap
    return tailProof
printProof [] [] _ = exitSuccess --not sure it's okay, but...

printProof' :: [(Expression, Info)] -> [(Int, Info)] -> Map.Map Int Int -> String
printProof' ((expr, info) : xsE) ((oldInd, _) : xsI) biMap = strToPrint ++ "\n" ++ printProof' xsE xsI biMap
    where   strToPrint = proofStr oldInd (expr, info) biMap
printProof' [] [] _ = "" 