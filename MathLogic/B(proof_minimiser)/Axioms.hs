module Axioms (
        axIndex
) where

import Grammar
import Lexer
import Parser
import Data.List
import Data.Maybe

isAxiom1 (Bin Impl a (Bin Impl b c)) = a == c
isAxiom1 _ = False

isAxiom2 (Bin Impl (Bin Impl a b) 
            (Bin Impl (Bin Impl c (Bin Impl d e))
                    (Bin Impl f g))) = a == c && a == f 
                                    && b == d && e == g
isAxiom2 _ = False 

isAxiom3 (Bin Impl a (Bin Impl b 
            (Bin Conj c d))) = a == c && b == d
isAxiom3 _ = False 

isAxiom4 (Bin Impl (Bin Conj a b) c) = a == c
isAxiom4 _ = False

isAxiom5 (Bin Impl (Bin Conj a b) c) = b == c
isAxiom5 _ = False

isAxiom6 (Bin Impl a (Bin Disj b c)) = a == b
isAxiom6 _ = False

isAxiom7 (Bin Impl a (Bin Disj b c)) = a == c
isAxiom7 _ = False

isAxiom8 (Bin Impl (Bin Impl a b)
                (Bin Impl (Bin Impl c d) 
                        (Bin Impl (Bin Disj e f)
                                g))) = a == e && b == d 
                                        && b == g && c == f
isAxiom8 _ = False

isAxiom9 (Bin Impl (Bin Impl a b)
                (Bin Impl (Bin Impl c (Not d)) 
                        (Not e))) = a == c && a == e
                                        && b == d
isAxiom9 _ = False

isAxiom10 (Bin Impl (Not (Not a)) b) = a == b
isAxiom10 _ = False

axIndex' :: Expression -> Maybe Int
axIndex' expr = (+1) <$> (True `elemIndex` isAx)
        where   axs = [isAxiom1, isAxiom2, isAxiom3, isAxiom4, isAxiom5,
                        isAxiom6, isAxiom7, isAxiom8, isAxiom9, isAxiom10]
                isAx = map (\isAx -> isAx expr) axs

--returns the number of axiom or 0
axIndex :: Expression -> Int
axIndex expr = fromMaybe 0 (axIndex' expr)