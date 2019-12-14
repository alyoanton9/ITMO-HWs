module Utils(
    parseExpr,
    detectType,
    deduceInIntuit,
    printHead,
    editMPMap
) where

import Grammar
import Axioms
import Deduce
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.List.Split
import Parser
import Lexer

data ExpressionType = AxHypo !Expression
    | Ax10 !Expression
    | MP !Expression !Expression
    deriving (Show, Eq)

parseExpr :: String -> Expression
parseExpr = parse . alexScanTokens

hypoIndex :: Expression -> [Expression] -> Int
hypoIndex expr hypos = fromMaybe 0 maybeInd
    where   isHypos = map (\hypo -> hypo == expr) hypos
            maybeInd = (+1) <$> (True `elemIndex` isHypos)

editMPMap :: Expression -> Map.Map Expression [Expression]
            -> Map.Map Expression [Expression]
editMPMap (Bin Impl base cons) mpMap = Map.insert cons newList mpMap
    where newList = case (Map.lookup cons mpMap) of
                        Nothing   -> [base]
                        Just list -> base : list
editMPMap _ mpMap = mpMap

--find stupid mistake!
detectType :: Expression -> [Expression] -> Set.Set Expression -> Map.Map Expression [Expression]
            -> ExpressionType
detectType expr hypos exSet 
                    mpMap = if (aInd == 10)
                                then (Ax10 expr)
                                else 
                                    if (aInd /= 0 || hInd /= 0)
                                        then (AxHypo expr)
                                        else (MP impl base)
    where   aInd = axIndex expr
            hInd = hypoIndex expr hypos
            base = fromMaybe (Var "") 
                    (find (\e -> Set.member e exSet) (mpMap Map.! expr)) --[Expression]
            impl = Bin Impl base expr

deduceAxHypo :: String -> [String] -> IO ()
deduceAxHypo exprStr (x : xs) = do
    putStrLn (replace1 'a' exprStr x "")
    deduceAxHypo exprStr xs 
deduceAxHypo _ [] = return ()

deduceModusPonens :: (String, String) -> [String] -> IO ()
deduceModusPonens (base, cons) (x : xs) = do
    putStrLn (replace2 'a' 'b' base cons x "")
    deduceModusPonens (base, cons) xs
deduceModusPonens _ [] = return ()

ax10Expr :: Expression -> Expression
ax10Expr (Bin Impl (Not (Not a)) b) = b
ax10Expr _ = Var ""

deduceLineByLine :: ExpressionType -> IO ()
deduceLineByLine eType = do
    case eType of
        (AxHypo expr) -> deduceAxHypo (show expr) classicAxHypo
        (Ax10 expr) -> deduceAxHypo (show (ax10Expr expr)) tenthAx
        (MP (Bin Impl base cons) base') -> deduceModusPonens (show base, 
                                                                show cons) modusPonens


deduceInIntuit :: Expression -> [Expression] -> Set.Set Expression
                -> Map.Map Expression [Expression] -> IO ()
deduceInIntuit expr hypos exSet mpMap = do
    let eType = detectType expr hypos exSet mpMap
    deduceLineByLine eType
 
printHead :: [Expression] -> Expression -> String
printHead hypos proving = head
    where   hyposStr = intercalate ", " (map show hypos)
            provingStr = show (Not (Not proving))
            head = hyposStr ++ " |- " ++ provingStr

replace1 :: Char -> String -> String -> String -> String
replace1 _ _ [] res = res
replace1 old new (x : xs) res = if (x == old)
                                    then replace1 old new xs (res ++ new)
                                    else replace1 old new xs (res ++ [x])

replace2 :: Char -> Char -> String -> String -> String -> String
            -> String
replace2 _ _ _ _ [] res  = res
replace2 old1 old2 new1 new2 
            (x : xs) res = if (x == old1)
                            then
                                replace2 old1 old2 new1 new2
                                                xs (res ++ new1)
                            else
                                if (x == old2)
                                    then 
                                        replace2 old1 old2 new1 new2
                                                xs (res ++ new2)
                                    else 
                                        replace2 old1 old2 new1 new2
                                                xs (res ++ [x])
