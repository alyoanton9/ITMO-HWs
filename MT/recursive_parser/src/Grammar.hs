module Grammar(
    Expression (..),
    evaluate
) where

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                | Neg Expression -- unary minus
                | Num Int
                deriving (Show, Eq)

evaluate :: Expression -> Int
evaluate (Add e1 e2)    = evaluate e1 + evaluate e2
evaluate (Sub e1 e2)    = evaluate e1 - evaluate e2
evaluate (Mul e1 e2)    = evaluate e1 * evaluate e2
evaluate (Div e1 e2)    = evaluate e1 `div` evaluate e2
evaluate (Mod e1 e2)    = evaluate e1 `mod` evaluate e2
evaluate (Neg e)        = - evaluate e
evaluate (Num v)        = v