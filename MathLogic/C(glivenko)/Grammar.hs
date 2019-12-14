module Grammar(
    BinaryOperation (..),
    Expression (..)
) where

data BinaryOperation = Conj | Disj | Impl deriving (Eq, Ord)
    
data Expression = Bin BinaryOperation Expression Expression
    | Not Expression
    | Var String
    deriving (Eq, Ord)
    
instance Show BinaryOperation where
    show Conj = "&"
    show Disj = "|"
    show Impl = "->"

instance Show Expression where
    show (Bin oper left right) = "(" ++ show left ++ " "
                                ++ show oper ++ " " ++ show right
                                ++ ")"
    show (Not (Var name)) = "!" ++ name 
    show (Not expr) = "!" ++ show expr
    show (Var name) = name

