{-# LANGUAGE InstanceSigs #-}

module Combinator(
    Parser (..),
    satisfy,
    eof,
    okay,
    chainl1
) where

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Tokenizer

-- our Parser has one field named runP, which is a function
newtype Parser a = Parser { runP :: [Token] -> Maybe (a, [Token]) } 

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser parse) = Parser $ \tokens -> fmap (first f)
                            (parse tokens)

instance Applicative Parser where
    pure :: a -> Parser a
    pure param = Parser $ \tokens -> Just (param, tokens)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser funcP <*> Parser aP = Parser $ \tokens -> case funcP tokens of
        Nothing             -> Nothing
        Just (func, tokens')   -> case aP tokens' of
            Nothing         -> Nothing
            Just (a, tokens'') -> Just (func a, tokens'')

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \tokens -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser aP1 <|> Parser aP2 = Parser $ \tokens -> aP1 tokens <|> aP2 tokens

instance Monad Parser where 
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> (Parser b)
    Parser aP >>= func =  Parser $ \tokens -> case (aP tokens) of
        Nothing             -> Nothing
        Just (res, remain)  -> (runP (func res)) remain


-- Returns 'default' parser if the string is empty
eof :: Parser ()
eof = Parser $ \tokens -> case tokens of
    [] -> Just((), [])
    _  -> Nothing

-- Parser that always works correct
okay :: Parser ()
okay = Parser $ \tokens -> Just((), tokens)

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = Parser $ \tokens -> case tokens of
    (t:ts)  -> if (predicate t)
                then Just(t, ts)
                else Nothing
    []      -> Nothing

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 opd opr = do 
                    a <- opd
                    remain a
    where remain a = (do 
                        func <- opr
                        b <- opd
                        remain $ func a b)
                    <|> return a
