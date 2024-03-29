module Deduce(
    classicAxHypo,
    tenthAx,
    modusPonens
) where

classicAxHypo :: [String]
classicAxHypo = ["a",
    "(a -> (!a -> a))",
    "(!a -> a)",
    "(!a -> (!a -> !a))",
    "((!a -> (!a -> !a)) -> ((!a -> ((!a -> !a) -> !a)) -> (!a -> !a)))",
    "((!a -> ((!a -> !a) -> !a)) -> (!a -> !a))",
    "(!a -> ((!a -> !a) -> !a))",
    "(!a -> !a)",
    "((!a -> a) -> ((!a -> !a) -> !!a))",
    "((!a -> !a) -> !!a)",
    "!!a"]

tenthAx :: [String]
tenthAx = ["(a -> (!!a -> a))",
 "((a -> (!!a -> a)) -> (!(!!a -> a) -> (a -> (!!a -> a))))",
 "((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))",
 "(((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> ((a -> (!!a -> a)) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))",
 "((a -> (!!a -> a)) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)))",
 "(((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))",
 "((((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)))) -> ((a -> (!!a -> a)) -> (((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))))",
 "((a -> (!!a -> a)) -> (((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)))))",
 "(((a -> (!!a -> a)) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (((a -> (!!a -> a)) -> (((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))))",
 "(((a -> (!!a -> a)) -> (((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)))))",
 "((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))))",
 "((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))",
 "(((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))))",
 "((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))))",
 "(((a -> (!!a -> a)) -> (!(!!a -> a) -> (a -> (!!a -> a)))) -> (((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))))",
 "(((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> (!!a -> a))) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))))",
 "((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))",
 "(((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a)))) -> (((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))))",
 "(((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> (!!a -> a)) -> ((a -> !(!!a -> a)) -> !a))) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))))",
 "((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)))",
 "(!(!!a -> a) -> (a -> !(!!a -> a)))",
 "((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> (a -> !(!!a -> a)))))",
 "((a -> (!!a -> a)) -> (!(!!a -> a) -> (a -> !(!!a -> a))))",
 "((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))",
 "(((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))))",
 "((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a))))",
 "(((a -> (!!a -> a)) -> (!(!!a -> a) -> (a -> !(!!a -> a)))) -> (((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))))",
 "(((a -> (!!a -> a)) -> ((!(!!a -> a) -> (a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))) -> ((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a))))",
 "((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a)))",
 "(((a -> (!!a -> a)) -> (!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a))) -> (((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> !a))))",
 "(((a -> (!!a -> a)) -> ((!(!!a -> a) -> ((a -> !(!!a -> a)) -> !a)) -> (!(!!a -> a) -> !a))) -> ((a -> (!!a -> a)) -> (!(!!a -> a) -> !a)))",
 "((a -> (!!a -> a)) -> (!(!!a -> a) -> !a))",
 "(!(!!a -> a) -> !a)",
 "(!a -> (!!a -> a))",
 "((!a -> (!!a -> a)) -> (!(!!a -> a) -> (!a -> (!!a -> a))))",
 "((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))",
 "(((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> ((!a -> (!!a -> a)) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "((!a -> (!!a -> a)) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)))",
 "(((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "((((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)))) -> ((!a -> (!!a -> a)) -> (((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))))",
 "((!a -> (!!a -> a)) -> (((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)))))",
 "(((!a -> (!!a -> a)) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (((!a -> (!!a -> a)) -> (((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))))",
 "(((!a -> (!!a -> a)) -> (((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)))))",
 "((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "(((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))))",
 "((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))))",
 "(((!a -> (!!a -> a)) -> (!(!!a -> a) -> (!a -> (!!a -> a)))) -> (((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))))",
 "(((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> (!!a -> a))) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))))",
 "((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "(((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a)))) -> (((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))))",
 "(((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> (!!a -> a)) -> ((!a -> !(!!a -> a)) -> !!a))) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))))",
 "((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)))",
 "(!(!!a -> a) -> (!a -> !(!!a -> a)))",
 "((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> (!a -> !(!!a -> a)))))",
 "((!a -> (!!a -> a)) -> (!(!!a -> a) -> (!a -> !(!!a -> a))))",
 "((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))",
 "(((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))))",
 "((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a))))",
 "(((!a -> (!!a -> a)) -> (!(!!a -> a) -> (!a -> !(!!a -> a)))) -> (((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))))",
 "(((!a -> (!!a -> a)) -> ((!(!!a -> a) -> (!a -> !(!!a -> a))) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))) -> ((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a))))",
 "((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a)))",
 "(((!a -> (!!a -> a)) -> (!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a))) -> (((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> !!a))))",
 "(((!a -> (!!a -> a)) -> ((!(!!a -> a) -> ((!a -> !(!!a -> a)) -> !!a)) -> (!(!!a -> a) -> !!a))) -> ((!a -> (!!a -> a)) -> (!(!!a -> a) -> !!a)))",
 "((!a -> (!!a -> a)) -> (!(!!a -> a) -> !!a))",
 "(!(!!a -> a) -> !!a)",
 "((!(!!a -> a) -> !a) -> ((!(!!a -> a) -> !!a) -> !!(!!a -> a)))",
 "((!(!!a -> a) -> !!a) -> !!(!!a -> a))",
 "!!(!!a -> a)"] 

modusPonens :: [String]
modusPonens = ["(a -> ((a -> b) -> a))",
 "((a -> ((a -> b) -> a)) -> (!b -> (a -> ((a -> b) -> a))))",
 "(!b -> (a -> ((a -> b) -> a)))",
 "((a -> b) -> ((a -> b) -> (a -> b)))",
 "(((a -> b) -> ((a -> b) -> (a -> b))) -> (!b -> ((a -> b) -> ((a -> b) -> (a -> b)))))",
 "(!b -> ((a -> b) -> ((a -> b) -> (a -> b))))",
 "(((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))",
 "((((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b))))) -> (!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))))",
 "(!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b))))))",
 "((!b -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))) -> (!b -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))))",
 "((!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))) -> (!b -> (a -> ((a -> b) -> ((a -> b) -> (a -> b))))))",
 "(!b -> (a -> ((a -> b) -> ((a -> b) -> (a -> b)))))",
 "(((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))",
 "((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "(!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))",
 "((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "(((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))) -> (!b -> ((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))))",
 "(!b -> ((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))))",
 "((!b -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> ((!b -> ((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))) -> (!b -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))))",
 "((!b -> ((((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))) -> (!b -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))))",
 "(!b -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "(((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))) -> (!b -> ((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))))",
 "(!b -> ((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))))",
 "((!b -> (a -> ((a -> b) -> ((a -> b) -> (a -> b))))) -> ((!b -> ((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))) -> (!b -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))))",
 "((!b -> ((a -> ((a -> b) -> ((a -> b) -> (a -> b)))) -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))) -> (!b -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))))",
 "(!b -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "((!b -> (a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))) -> ((!b -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))) -> (!b -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))))",
 "((!b -> ((a -> (((a -> b) -> ((a -> b) -> (a -> b))) -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))) -> (!b -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))))",
 "(!b -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))))",
 "((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))",
 "(((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (!b -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))",
 "(!b -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))))",
 "(((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))",
 "((((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))))) -> (!b -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))))",
 "(!b -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))))))",
 "((!b -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((!b -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))) -> (!b -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))))",
 "((!b -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))) -> (!b -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))))))",
 "(!b -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))))",
 "((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))",
 "(((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b))))) -> (!b -> ((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))))",
 "(!b -> ((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b))))))",
 "((!b -> (a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))))) -> ((!b -> ((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))) -> (!b -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))))",
 "((!b -> ((a -> ((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b)))) -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))) -> (!b -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b))))))",
 "(!b -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b)))))",
 "((!b -> (a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b))))) -> ((!b -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b))))) -> (!b -> (a -> ((a -> b) -> (a -> b))))))",
 "((!b -> ((a -> (((a -> b) -> (((a -> b) -> (a -> b)) -> (a -> b))) -> ((a -> b) -> (a -> b)))) -> (a -> ((a -> b) -> (a -> b))))) -> (!b -> (a -> ((a -> b) -> (a -> b)))))",
 "(!b -> (a -> ((a -> b) -> (a -> b))))",
 "(((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))",
 "((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (!b -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "(!b -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))",
 "((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "(((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))) -> (!b -> ((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))))",
 "(!b -> ((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))))",
 "((!b -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> ((!b -> ((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))) -> (!b -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))))",
 "((!b -> ((((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))) -> (!b -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))))",
 "(!b -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "(((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))) -> (!b -> ((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))))",
 "(!b -> ((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))))",
 "((!b -> (a -> ((a -> b) -> a))) -> ((!b -> ((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))) -> (!b -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))))",
 "((!b -> ((a -> ((a -> b) -> a)) -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))) -> (!b -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))))",
 "(!b -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "((!b -> (a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))) -> ((!b -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))) -> (!b -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))))",
 "((!b -> ((a -> (((a -> b) -> a) -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))) -> (!b -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))))",
 "(!b -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))))",
 "((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))",
 "(((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b)))) -> (!b -> ((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))))",
 "(!b -> ((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b)))))",
 "((!b -> (a -> ((a -> b) -> (a -> b)))) -> ((!b -> ((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))) -> (!b -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))))",
 "((!b -> ((a -> ((a -> b) -> (a -> b))) -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))) -> (!b -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b)))))",
 "(!b -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b))))",
 "((!b -> (a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b)))) -> ((!b -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b)))) -> (!b -> (a -> ((a -> b) -> b)))))",
 "((!b -> ((a -> (((a -> b) -> (a -> b)) -> ((a -> b) -> b))) -> (a -> ((a -> b) -> b)))) -> (!b -> (a -> ((a -> b) -> b))))",
 "(!b -> (a -> ((a -> b) -> b)))",
 "(((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))",
 "((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (!b -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))",
 "(!b -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))))",
 "((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))",
 "(((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))))) -> (!b -> ((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))))",
 "(!b -> ((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))))))",
 "((!b -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> ((!b -> ((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))) -> (!b -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))))",
 "((!b -> ((((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))) -> (!b -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))))))",
 "(!b -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))))",
 "(!b -> ((a -> b) -> !b))",
 "((!b -> ((a -> b) -> !b)) -> (!b -> (!b -> ((a -> b) -> !b))))",
 "(!b -> (!b -> ((a -> b) -> !b)))",
 "((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b))))",
 "(((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b)))) -> (!b -> ((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b))))))",
 "(!b -> ((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b)))))",
 "((!b -> (!b -> ((a -> b) -> !b))) -> ((!b -> ((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b))))) -> (!b -> (a -> (!b -> ((a -> b) -> !b))))))",
 "((!b -> ((!b -> ((a -> b) -> !b)) -> (a -> (!b -> ((a -> b) -> !b))))) -> (!b -> (a -> (!b -> ((a -> b) -> !b)))))",
 "(!b -> (a -> (!b -> ((a -> b) -> !b))))",
 "(!b -> (a -> !b))",
 "((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))",
 "(((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b)))) -> (!b -> ((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))))",
 "(!b -> ((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b)))))",
 "((!b -> (a -> !b)) -> ((!b -> ((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))) -> (!b -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))))",
 "((!b -> ((a -> !b) -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))) -> (!b -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b)))))",
 "(!b -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b))))",
 "((!b -> (a -> (!b -> ((a -> b) -> !b)))) -> ((!b -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b)))) -> (!b -> (a -> ((a -> b) -> !b)))))",
 "((!b -> ((a -> (!b -> ((a -> b) -> !b))) -> (a -> ((a -> b) -> !b)))) -> (!b -> (a -> ((a -> b) -> !b))))",
 "(!b -> (a -> ((a -> b) -> !b)))",
 "((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))",
 "(((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b))))) -> (!b -> ((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))))",
 "(!b -> ((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b))))))",
 "((!b -> (a -> ((a -> b) -> b))) -> ((!b -> ((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))) -> (!b -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))))",
 "((!b -> ((a -> ((a -> b) -> b)) -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))) -> (!b -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b))))))",
 "(!b -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b)))))",
 "((!b -> (a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b))))) -> ((!b -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b))))) -> (!b -> (a -> (((a -> b) -> !b) -> !(a -> b))))))",
 "((!b -> ((a -> (((a -> b) -> b) -> (((a -> b) -> !b) -> !(a -> b)))) -> (a -> (((a -> b) -> !b) -> !(a -> b))))) -> (!b -> (a -> (((a -> b) -> !b) -> !(a -> b)))))",
 "(!b -> (a -> (((a -> b) -> !b) -> !(a -> b))))",
 "((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))",
 "(((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b)))) -> (!b -> ((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))))",
 "(!b -> ((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b)))))",
 "((!b -> (a -> ((a -> b) -> !b))) -> ((!b -> ((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))) -> (!b -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))))",
 "((!b -> ((a -> ((a -> b) -> !b)) -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))) -> (!b -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b)))))",
 "(!b -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b))))",
 "((!b -> (a -> (((a -> b) -> !b) -> !(a -> b)))) -> ((!b -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b)))) -> (!b -> (a -> !(a -> b)))))",
 "((!b -> ((a -> (((a -> b) -> !b) -> !(a -> b))) -> (a -> !(a -> b)))) -> (!b -> (a -> !(a -> b))))",
 "(!b -> (a -> !(a -> b)))",
 "((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a))",
 "(((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a)) -> (!b -> ((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a))))",
 "(!b -> ((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a)))",
 "(!!(a -> b) -> (a -> !!(a -> b)))",
 "((!!(a -> b) -> (a -> !!(a -> b))) -> (!b -> (!!(a -> b) -> (a -> !!(a -> b)))))",
 "(!b -> (!!(a -> b) -> (a -> !!(a -> b))))",
 "!!(a -> b)",
 "(!!(a -> b) -> (!b -> !!(a -> b)))",
 "(!b -> !!(a -> b))",
 "((!b -> !!(a -> b)) -> ((!b -> (!!(a -> b) -> (a -> !!(a -> b)))) -> (!b -> (a -> !!(a -> b)))))",
 "((!b -> (!!(a -> b) -> (a -> !!(a -> b)))) -> (!b -> (a -> !!(a -> b))))",
 "(!b -> (a -> !!(a -> b)))",
 "((!b -> (a -> !(a -> b))) -> ((!b -> ((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a))) -> (!b -> ((a -> !!(a -> b)) -> !a))))",
 "((!b -> ((a -> !(a -> b)) -> ((a -> !!(a -> b)) -> !a))) -> (!b -> ((a -> !!(a -> b)) -> !a)))",
 "(!b -> ((a -> !!(a -> b)) -> !a))",
 "((!b -> (a -> !!(a -> b))) -> ((!b -> ((a -> !!(a -> b)) -> !a)) -> (!b -> !a)))",
 "((!b -> ((a -> !!(a -> b)) -> !a)) -> (!b -> !a))",
 "(!b -> !a)",
 "((!b -> !a) -> ((!b -> !!a) -> !!b))",
 "(!!a -> (!b -> !!a))",
 "!!a",
 "(!b -> !!a)",
 "((!b -> !!a) -> !!b)",
 "!!b"]