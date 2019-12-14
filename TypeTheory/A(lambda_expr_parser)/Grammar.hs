module Grammar where

type Variable = String

data Expression = Lambda Variable Expression 
                    | Apply Expression Expression
                    | Var Variable

instance Show Expression where
    show (Lambda var expr) = "(\\" ++ var ++ "." ++ show expr ++ ")"
    show (Apply expr1 expr2) = "(" ++ show expr1 ++ " " 
                                    ++ show expr2 ++ ")" 
    show (Var var) = var