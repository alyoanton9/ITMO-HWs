module Tokenizer(
    Token (OpParT, ClParT, AddT, SubT, MulT, DivT, ModT, NegT, NumT, ErrorT),
    getNum,
    tokenize
) where

import Data.Char

data Token  = OpParT
            | ClParT
            | AddT
            | SubT
            | MulT
            | DivT
            | ModT
            | NegT
            | NumT Int
            | ErrorT String
            deriving Eq

instance Show Token where
    show OpParT = "("
    show ClParT = ")"
    show AddT = "+"
    show SubT = "-"
    show MulT = "*"
    show DivT = "/"
    show ModT = "%"
    show NegT = "-"
    show (NumT n) = show n
    show (ErrorT e) = e

-- Unary minus can be only when previous token is OpParT or Nothing
getInt' :: String -> String -> (Int, String)
getInt' (c:cs) digits = if (isDigit c)
                            then getInt' cs $ digits ++ [c]
                            else (read digits, (c:cs))
getInt' "" digits = (read digits, "")                         

-- returns read number and remained string
getInt :: Bool -> String -> (Int, String)
getInt neg str = if neg
                    then (negate num, remain)
                    else (num, remain)
    where (num, remain) = getInt' str ""

tokenize' :: String -> [Token] -> [Token]
tokenize' (c:cs) tokens
    | c == '(' = tokenize' cs $ tokens ++ [OpParT]
    | c == ')' = tokenize' cs $ tokens ++ [ClParT]
    | c == '+' = tokenize' cs $ tokens ++ [AddT]
    | c == '-' = if (null tokens || last tokens == OpParT)
                    then
                        if (isDigit $ head cs)
                            then let (num, remain) = getInt True cs in
                                    tokenize' remain $ tokens ++ [NumT num]
                            else 
                                if (head cs == '(')
                                    then tokenize' cs $ tokens ++ [NegT]
                                    else tokens ++ [ErrorT ("Unpredictable symbol " ++ [head cs])]
                    else
                        tokenize' cs $ tokens ++ [SubT]
    | c == '*' = tokenize' cs $ tokens ++ [MulT]
    | c == '/' = tokenize' cs $ tokens ++ [DivT]
    | c == '%' = tokenize' cs $ tokens ++ [ModT]
    | isDigit c = let (num, remain) = getInt False (c:cs) in
                    tokenize' remain $ tokens ++ [NumT num]
    | isSpace c = tokenize' cs $ tokens
    | otherwise = tokens ++ [ErrorT ("Unpredictable symbol " ++ [c])]
tokenize' "" tokens = tokens

tokenize :: String -> [Token]
tokenize = flip tokenize' $ [] 

getNum :: Token -> Int
getNum (NumT n) = n
getNum _        = 0