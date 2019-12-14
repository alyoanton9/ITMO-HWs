module TT(
    makeTT,
    evaluate,
    getVars,
    findMinSet,
    reverseTT,
    isPositive,
    isUnProovable,
    negVars
) where

import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Grammar
import Parser
import TTValues
import Utils

defPair :: (String, Bool)
defPair = ("", False)

type TT = [([Bool], Bool)]
defExpr = Var ""

-- may be rewrite
evaluate :: Set.Set (String, Bool) -> Expression -> Bool
evaluate valueSet (Bin Impl a b) = not (evaluate valueSet a) || (evaluate valueSet b)
evaluate valueSet (Bin Disj a b) = (evaluate valueSet a) || (evaluate valueSet b)
evaluate valueSet (Bin Conj a b) = (evaluate valueSet a) && (evaluate valueSet b)
evaluate valueSet (Not a)        = not (evaluate valueSet a)
evaluate valueSet (Var a)        = aValue
        where   aValue = snd (fromMaybe defPair 
                                (find (\(s, b) -> s == a) (Set.toList valueSet)))

getVars :: Expression -> Set.Set String -> Set.Set String
getVars (Bin _ a b) set = Set.union (getVars a set) (getVars b set)
getVars (Not a) set = Set.union (getVars a set) set
getVars (Var a) set = Set.insert a set

-- [True, True, False]

makeLineTT :: Expression -> [String] -> [Bool] -> ([Bool], Bool)
makeLineTT expr varsList values = (values, evaluate varsValues expr)
    where varsValues = Set.fromList (zip varsList values) --[(A, True), (B, False)...]
            
makeTT :: Expression -> [([Bool], Bool)]
makeTT expr = wholeTT
    where   varsList = Set.toList (getVars expr Set.empty)
            valuesTT = case (length varsList) of
                        1 -> valuesTT1
                        2 -> valuesTT2
                        3 -> valuesTT3
            wholeTT = map (\valueLine -> makeLineTT expr varsList valueLine) valuesTT

alwaysTrue :: TT -> Bool -> Bool
alwaysTrue (tt : tts) res = alwaysTrue tts (res && snd tt)
alwaysTrue [] res = res

--function in arguments means should we look at this list or not
canBeMinSet :: ([Bool] -> Bool) -> TT -> Bool -> Bool
canBeMinSet predicate (vals : valss) res = if (predicate . fst $ vals)
                                            then canBeMinSet predicate valss (res && snd vals)
                                            else canBeMinSet predicate valss (res && True)
canBeMinSet _ [] res = res


findMinSet1 :: [Int] -> [String] -> TT -> Set.Set String
                -> Set.Set String
findMinSet1 (i : is) vars tt set = findMinSet1 is vars tt newSet
    where   predicate values = (values !! i) == True
            isMinSet = canBeMinSet predicate tt True
            newSet = if isMinSet
                        then Set.insert (vars !! i) set
                        else set
findMinSet1 [] _ _ set = set

findMinSet2 :: [(Int, Int)] -> [String] -> TT -> Set.Set (String, String)
                -> Set.Set (String, String)
findMinSet2 (i : is) vars tt set = findMinSet2 is vars tt newSet
    where   predicate values = ((values !! fst i) && (values !! snd i)) == True
            isMinSet = canBeMinSet predicate tt True
            newSet = if isMinSet 
                        then Set.insert ((vars !! fst i), (vars !! snd i)) set
                        else set
findMinSet2 [] _ _ set = set

--not really beautiful...
--now works only if the last str in tt is truth
findMinSet :: [String] -> TT -> Set.Set String
findMinSet vars tt = resultSet
    where   is1 = case (length vars) of
                    1 -> [0]
                    2 -> [0..1]
                    3 -> [0..2]
            is2 = case (length vars) of 
                    1 -> [(0, 0)] -- can't be!!
                    2 -> [(0, 1)]
                    3 -> [(0, 1), (0, 2), (1, 2)]
            set1 = findMinSet1 is1 vars tt Set.empty
            set2 = findMinSet2 is2 vars tt Set.empty
            pair2 = head (Set.toList set2)
            resultSet = if (alwaysTrue tt True)
                            then Set.empty
                            else 
                                if not (null set1) --(findMinSet1 is1 vars tt set))
                                    then Set.insert (head (Set.toList set1)) Set.empty
                                    else 
                                        if not (null set2)
                                            then Set.insert (fst pair2) (Set.insert (snd pair2) Set.empty) --head (Set.toList set2)
                                            else
                                                Set.fromList (vars)

reverseTT :: TT -> TT
reverseTT tt = zip vValues (reverse eValues)
    where   eValues = map (\t -> snd t) tt
            vValues = map (\t -> fst t) tt

negVars :: Expression -> Expression
negVars (Bin Impl a b) = Bin Impl (negVars a) (negVars b)
negVars (Bin Disj a b) = Bin Disj (negVars a) (negVars b)
negVars (Bin Conj a b) = Bin Conj (negVars a) (negVars b)
negVars (Not a) = Not (negVars a)
negVars (Var var) = Not (Var var)


isPositive :: TT -> Bool
isPositive tt = snd . last $ tt

isUnProovable :: TT -> Bool
isUnProovable tt = not (snd . last $ tt) && (snd . head $ tt)

showTT :: [([Bool], Bool)] -> String -> String
showTT (tt : tts) res = showTT tts (res ++ show tt ++ "    ")
showTT [] res = res 