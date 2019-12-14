module Visual(
    makeExprGraph,
    exprGraphParams,
    makeDotGraph
) where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Grammar as Gr
import           Tokenizer
import qualified ParserImpl as P

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           System.FilePath.Posix
import           System.Directory
import           System.IO

data VLabel = VLAdd
            | VLSub
            | VLMul
            | VLDiv
            | VLMod
            | VLNeg
            | VLNum Int
            deriving Eq

instance Show VLabel where
    show VLAdd          = "+"
    show VLSub          = "-"
    show VLMul          = "*"
    show VLDiv          = "/"
    show VLMod          = "%"
    show VLNeg          = "neg"
    show (VLNum num)    = show num  


-- Int is a vertex's number
type V = (Int, VLabel)

-- Edges don't need labels
type E = (Int, Int, ())

type ExprGraph = ([V], [E])

exprGraphParams :: G.GraphvizParams Int VLabel () () VLabel
exprGraphParams = G.nonClusteredParams {G.fmtNode = \(_, vl)
                                            -> [G.toLabel . show $ vl]}

-- looks awful, but I was lazy to think about beauty
makeVE :: Gr.Expression -> Int -> [V] -> [E] -> ([V], [E], Int)
makeVE (Gr.Add e1 e2)
        count vs es =   let (vs1, es1, count') = makeVE e1 (count + 1) (vs ++
                                                [(count, VLAdd)]) (es ++ [(count, count + 1, ())])
                        in makeVE e2 count' vs1 (es1 ++ [(count, count', ())])
makeVE (Gr.Sub e1 e2)
        count vs es =   let (vs1, es1, count') = makeVE e1 (count + 1) (vs ++ [(count, VLSub)]) (es ++
                                                [(count, count + 1, ())])
                        in makeVE e2 count' vs1 (es1 ++ [(count, count', ())])
makeVE (Gr.Mul e1 e2)
        count vs es =   let (vs1, es1, count') = makeVE e1 (count + 1) (vs ++ [(count, VLMul)]) (es ++
                                                [(count, count + 1, ())])
                        in makeVE e2 count' vs1 (es1 ++ [(count, count', ())])
makeVE (Gr.Div e1 e2)
        count vs es =   let (vs1, es1, count') = makeVE e1 (count + 1) (vs ++ [(count, VLDiv)]) (es ++
                                                [(count, count + 1, ())])
                        in makeVE e2 count' vs1 (es1 ++ [(count, count', ())])
makeVE (Gr.Mod e1 e2)
        count vs es =   let (vs1, es1, count') = makeVE e1 (count + 1) (vs ++ [(count, VLMod)]) (es ++
                                                [(count, count + 1, ())])
                        in makeVE e2 count' vs1 (es1 ++ [(count, count', ())])
makeVE (Gr.Neg e)
        count vs es = makeVE e (count + 1) (vs ++ [(count, VLNeg)])
                                    (es ++ [(count, count + 1, ())])
makeVE (Gr.Num n) count vs es = (vs ++ [(count, VLNum n)], es, count + 1)

makeExprGraph :: Gr.Expression -> ExprGraph
makeExprGraph expr = let (vs, es, vnum) = makeVE expr
                                            0 [] [] in (vs, es)

makeDotGraph :: ExprGraph -> String -> IO()
makeDotGraph (vs, es) filename = do
    let dotGraph = G.graphElemsToDot exprGraphParams vs es :: G.DotGraph Int
        dotText = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile ("trees\\" ++ filename ++ ".dot") dotText
