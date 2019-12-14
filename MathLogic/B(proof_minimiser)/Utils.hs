module Utils(
    Info (AxInfo, HypoInfo, MPInfo),
    Determ (Final, Possible),
    hypoIndex,
    exprIndex,
    mpIndexes,
    addAx,
    addHypo,
    addMP,
    editMPMap'',
    editMPMap',
    defaultExpr,
    defaultMap,
    parseExpr,
    tickUsed,
    filterProof,
    bijectiveNums
) where
import Grammar
import Lexer
import Parser
import Axioms
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import qualified Data.Text as T

data Info = AxInfo Int  
    | HypoInfo Int
    | MPInfo (Int, Int)
    deriving (Eq, Show)

data Determ = Final Expression
    | Possible (Set.Set Expression)
    deriving (Eq, Show)

defaultExpr = Var ""
defaultPair = (defaultExpr, defaultExpr)
defaultMap = Map.empty

hypoIndex :: Expression -> [Expression] -> Int
hypoIndex expr hypos = fromMaybe 0 maybeInd
    where   isHypos = map (\hypo -> hypo == expr) hypos
            maybeInd = (+1) <$> (True `elemIndex` isHypos)

--O(logn)
exprIndex :: Expression -> Map.Map Expression Int -> Int
exprIndex expr eMap = fromMaybe 0 (exprIndex' expr eMap)
    where exprIndex' e m = Map.lookup e m

memberOfProof :: Map.Map Expression Int -> Expression -> Bool
memberOfProof eMap expr = Map.member expr eMap

editMPMap' :: Expression -> Map.Map Expression Int -> Map.Map Expression Determ
                -> Map.Map Expression Determ
editMPMap' (Bin Impl left right) 
            eMap mpMap = case (Map.lookup right mpMap) of 
                            Just (Final _)           -> mpMap
                            Just (Possible baseList) -> Map.insert right (newDeterm baseList) mpMap
                            Nothing                  -> Map.insert right (newDeterm Set.empty) mpMap
            where newDeterm bL = if Map.member left eMap --expr was in proof earlier
                                    then (Final left)
                                    else (Possible (Set.insert left bL))
--new expr can be just a base 
editMPMap' _ _ mpMap = mpMap

setOfDeterms :: Determ -> Set.Set Expression
setOfDeterms (Final l)    = Set.insert l Set.empty
setOfDeterms (Possible l) = l

editMPMap'' :: Expression -> Map.Map Expression Int -> Map.Map Expression Determ
                -> (Map.Map Expression Determ, Expression)
editMPMap'' expr eMap mpMap = if length bases == 0
                                then (mpMap, defaultExpr)
                                else ((Map.insert expr (Final (head bases))
                                        mpMap), head bases)
    where   posBases = case Map.lookup expr mpMap of -- 
                        Nothing         -> Set.empty
                        Just a          -> setOfDeterms a
            bases = filter (\e -> Map.member e eMap) (Set.toList posBases)

--base is a base expression (A in (A, A->B => B))
mpIndexes :: Expression -> Expression -> Map.Map Expression Int
            -> (Int, Int)
mpIndexes base expr eMap = (implInd, baseInd)
    where   baseInd = exprIndex base eMap
            implInd = exprIndex (Bin Impl base expr) eMap


addNewExpr :: Info -> Int -> Expression -> Map.Map Expression Int -> [(Expression, Info)]
                -> (Map.Map Expression Int, [(Expression, Info)], Bool)
addNewExpr info ind expr exprMap infoList = if isJust (Map.lookup expr exprMap) --O(logn)
                                                then (exprMap, infoList, False) --O(1)
                                                else (newExprMap, (expr, info) : infoList, True) --adding list is a an exprensive operation 
    where newExprMap = Map.insert expr ind exprMap -- O(logn)

addAx :: Int -> Int -> Expression -> Map.Map Expression Int -> [(Expression, Info)]
            -> (Map.Map Expression Int, [(Expression, Info)], Bool)
addAx axInd = addNewExpr (AxInfo axInd)

addHypo hInd = addNewExpr (HypoInfo hInd)

addMP :: (Int, Int) -> Int -> Expression -> Map.Map Expression Int -> [(Expression, Info)]
            -> (Map.Map Expression Int, [(Expression, Info)], Bool)
addMP inds = addNewExpr (MPInfo inds)

implParts :: Expression -> (Expression, Expression)
implParts (Bin Impl a b) = (a, b)
implParts _ = defaultPair

parseExpr :: String -> Expression
parseExpr str = parse . alexScanTokens $ str

recurTick ::  Set.Set Int -> [(Expression, Info)] -> Map.Map Expression Int
            -> Set.Set Int
recurTick set [] exprMap = set
recurTick set ((expr, MPInfo (i1, i2)) : xs)
            exprMap = if (Set.member eInd set)
                        then recurTick 
                                (Set.insert i1 (Set.insert i2 set)) xs exprMap
                        else recurTick set xs exprMap
    where   eInd = exprIndex expr exprMap
recurTick set (x : xs) exprMap = recurTick set xs exprMap

--do reverse of info twice...
tickUsed :: Expression -> Map.Map Expression Int -> [(Expression, Info)] -> Set.Set Int
tickUsed last exprMap infoList = recurTick (Set.insert (exprIndex last exprMap)
                                        Set.empty) (reverse infoList) exprMap

filterProof :: Map.Map Expression Int -> [(Expression, Info)] -> [(Expression, Info)] 
            -> [(Int, Info)] -> Set.Set Int -> ([(Expression, Info)], [(Int, Info)])
filterProof exprMap ((expr, info) : xs) 
            resInfoE resInfoI set = if (Set.member eInd set) --O(logn)
                                        then filterProof exprMap xs 
                                            ((expr, info) : resInfoE)
                                            ((eInd, info) : resInfoI) set
                                        else filterProof exprMap xs
                                            resInfoE resInfoI set  
    where eInd = exprIndex expr exprMap
filterProof exprMap [] resInfoE resInfoI set = (resInfoE, resInfoI)

bijectiveNums :: [(Int, Info)] -> Map.Map Int Int
bijectiveNums infoList = Map.fromList (map (\((oldInd, _), newInd)
                                            -> (oldInd, newInd))
                                                (zip infoList [1..]))