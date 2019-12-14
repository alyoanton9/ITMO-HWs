module Utils(
    parseExpr,
    replace1,
    replace2,
    replace3,
    join
) where

import Lexer
import Parser
import Grammar
import Data.List

parseExpr :: String -> Expression
parseExpr = parse . alexScanTokens

replace1 :: Char -> String -> String -> String -> String
replace1 _ _ res [] = res
replace1 old new res (x : xs) = if (x == old)
                                    then replace1 old new (res ++ new) xs
                                    else replace1 old new (res ++ [x]) xs

replace2 :: Char -> Char -> String -> String -> String -> String
            -> String
replace2 _ _ _ _ res [] = res
replace2 old1 old2 new1 new2 
            res (x : xs) = if (x == old1)
                            then
                                replace2 old1 old2 new1 new2
                                                (res ++ new1) xs
                            else
                                if (x == old2)
                                    then 
                                        replace2 old1 old2 new1 new2
                                                (res ++ new2) xs
                                    else 
                                        replace2 old1 old2 new1 new2
                                                (res ++ [x]) xs
--may be slow, but need only for mp
replace3 :: Char -> Char -> Char -> String -> String -> String
            -> String -> String -> String
replace3 _ _ _ _ _ _ res [] = res
replace3 old1 old2 old3
            new1 new2 new3 
            res (x : xs) = if (x == old1)
                            then replace3 old1 old2 old3 
                                        new1 new2 new3 (res ++ new1) xs
                            else 
                                if (x == old2)
                                    then replace3 old1 old2 old3 
                                            new1 new2 new3 (res ++ new2) xs
                                    else
                                        if (x == old3)
                                            then replace3 old1 old2 old3 
                                                        new1 new2 new3 (res ++ new3) xs
                                            else replace3 old1 old2 old3 
                                                        new1 new2 new3 (res ++ [x]) xs

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)