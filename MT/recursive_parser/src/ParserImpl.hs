module ParserImpl(
    parse,
    run
) where

import Combinator
import Grammar
import Data.Char
import Control.Applicative
import Control.Arrow
import Tokenizer


tokenPred :: Token -> Token -> Bool
tokenPred (NumT _) (NumT _) = True
tokenPred (ErrorT _) (ErrorT _) = True
tokenPred t1 t2 = t1 == t2

parseNumber' :: Parser Int
parseNumber' = do
    numT <- parseToken $ NumT 0 
    return $ getNum numT

parseNumber :: Parser Expression
parseNumber = do
    num <- parseNumber'
    return $ Num num

parseToken :: Token -> Parser Token
parseToken t = satisfy $ tokenPred t

parseParenthNeg :: Parser Expression -> Parser Expression
parseParenthNeg p = do
    neg <- parseNeg
    parseToken OpParT
    e <- p
    parseToken ClParT
    return $ neg e

parseParenthPos :: Parser Expression -> Parser Expression
parseParenthPos p = do
    parseToken OpParT
    e <- p 
    parseToken ClParT
    return $ e

parseUnOper :: Token -> (a -> a) -> Parser (a -> a)
parseUnOper t op = parseToken t >> return op

parseBinOper :: Token -> (a -> a -> a) -> Parser (a -> a -> a)
parseBinOper t op = parseToken t >> return op

parseNeg :: Parser (Expression -> Expression)
parseNeg = parseUnOper NegT Neg

parseAddSub :: Parser (Expression -> Expression -> Expression)
parseAddSub = (parseBinOper AddT Add) <|> (parseBinOper SubT Sub)   -- E' -> +TE' | -TE' | eps
--FIRST:    +, -, eps 
--FOLLOW:   ), $

parseMulDivMod :: Parser (Expression -> Expression -> Expression)   -- T' -> *FT' | /FT' | %FT' | eps
parseMulDivMod = (parseBinOper MulT Mul) <|> (parseBinOper DivT Div) <|> (parseBinOper ModT Mod)
--FIRST:    *, /, %, eps
--FOLLOW:   +, -, $, )

runParser :: Parser a -> [Token] -> a
runParser p tokens = case (runP p tokens) of
    Just (res, remain)  -> if (null remain)
                            then res
                            else error $ "Inappropriate remains: " ++ show remain
    Nothing             -> error "Parse error"

expr :: Parser Expression
expr = term `chainl1` parseAddSub   -- E -> TE' 
--FIRST:    n, -, (
--FOLLOW:   $, )

term :: Parser Expression
term = factor `chainl1` parseMulDivMod  -- T -> FT'
--FIRST:    n, -, (
--FOLLOW:   +, -, $, )

factor :: Parser Expression
factor = parseNumber <|> parseParenthPos expr <|> parseParenthNeg expr  -- F -> n | (E) | -F
--FIRST:    n, -, (
--FOLLOW:   +, -, *, /, %, ), $

parse :: [Token] -> Expression
parse = runParser expr

run :: String -> Int
run str = evaluate . parse . tokenize $ str