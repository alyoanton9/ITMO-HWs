module InfoMaker(
    Info (AxHypo, MP)
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Grammar
import Axioms
import Lexer
import Parser
import ElemProofs

parseExpr = parse . alexScanTokens

data Info = AxHypo | MP String  deriving Show

hypoIndex :: Expression -> [Expression] -> Int
hypoIndex expr hypos = fromMaybe 0 maybeInd
    where maybeInd = (+1) <$> (expr `elemIndex` hypos)


addToMP :: Expression -> Map.Map Expression [Expression] 
            -> Map.Map Expression [Expression]
addToMP (Bin Impl base cons) mpMap = Map.insert cons newBases mpMap
    where newBases = case (Map.lookup cons mpMap) of
                        Nothing     -> [base]
                        Just a      -> base : a
addToMP _ mpMap = mpMap

--problems with String
getInfo :: Expression -> [Expression] -> Set.Set Expression
            -> Map.Map Expression [Expression] -> Info
getInfo expr hypos exprSet 
                mpMap = if (aInd /= 0 || hInd /= 0)
                            then AxHypo
                            else (MP base)
    where   aInd = axIndex expr
            hInd = hypoIndex expr hypos
            base' = fromMaybe (Var "") (find (\e -> Set.member e exprSet)
                                (mpMap Map.! expr)) --[Expression]
            base = show base'

addInfo :: [String] -> [Expression] -> Set.Set Expression 
            -> Map.Map Expression [Expression] -> [(String, Info)] -> [(String, Info)]
addInfo (t : ts) hypos exprSet mpMap 
                infoTemplate = addInfo ts hypos exprSet' mpMap' (infoTemplate ++ tInfo)
    where   tExpr = parseExpr t
            tInfo = [(t, getInfo tExpr hypos exprSet mpMap)]
            exprSet' = Set.insert tExpr exprSet
            mpMap' = addToMP tExpr mpMap
addInfo [] _ _ _ infoTemplate = infoTemplate

sh :: [(String, Info)] -> IO ()
sh (s : ss) = do
    putStr . show $ s
    putStrLn ","
    sh ss
sh [] = return ()