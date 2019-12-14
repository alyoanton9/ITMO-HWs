module Grammar where


data BinaryOperation = Conj | Disj | Impl

data Expression = Bin BinaryOperation Expression Expression
    | Not Expression
    | Var String

instance Show BinaryOperation where
    show Conj = "&"
    show Disj = "|"
    show Impl = "->"

instance Show Expression where
    show (Bin oper left right) = "(" ++ show oper ++ ","
                                 ++ show left ++ ","
                                 ++ show right ++ ")"
    show (Not expr) = "(!" ++ show expr ++ ")"
    show (Var name) = name
