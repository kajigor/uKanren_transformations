module TreePrinter where 

import Data.Text.Lazy (Text, pack, unpack)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (
  GraphvizParams,
  GlobalAttributes(
    GraphAttrs,
    NodeAttrs
    ),
  X11Color(Transparent, White),
  nonClusteredParams,
  globalAttributes,
  fmtNode,
  fmtEdge,
  graphToDot
  )
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete (
  Attribute(
    BgColor,
    Shape,
    Label,
    ViewPort,
    RankDir,
    Style,
    FillColor
    ),
  Shape(BoxShape),
  Label(StrLabel),
  RankDir(FromTop),
  StyleName(Filled),
  StyleItem(SItem),
  toWColor,
  )

import qualified Eval as E
import Syntax
import Tree

treeToGraph :: Tree -> Gr Text Text
treeToGraph tree = 
  let (vs, es) = label tree 
  in  mkGraph (map (\(i, v) -> (i, pack ("<" ++ v ++ ">"))) vs) (map (\(i,j,l) -> (i,j, pack l)) es)

label :: Tree -> ([(Int,String)], [(Int,Int,String)])
label tree = 
  label' tree 1 [] []
  where 
    addLeaf  i n ns es = ((i, n) : ns, es)
    addChild i n ns es ch = 
      let i' = 2*i
          (ns', es') = label' ch i' ns es 
      in  ((i, n) : ns', (i, i', "") : es')
    addChildren i n ns es ch1 ch2 = 
      let i1 = 2*i
          i2 = i1 + 1
          (ns',  es')  = label' ch1 i1 ns es
          (ns'', es'') = label' ch2 i2 ns es
      in  ((i, n) : (ns' ++ ns''), (i, i1, "") : (i, i2, "") : (es' ++ es''))
    label' t@Fail                         i ns es = addLeaf     i (showNode t) ns es
    label' t@(Success _)                  i ns es = addLeaf     i (showNode t) ns es
    label' t@(Rename _ _ _)               i ns es = addLeaf     i (showNode t) ns es
    label' t@(Gen _ _ ch _)               i ns es = addChild    i (showNode t) ns es ch
    label' t@(Or ch1 ch2 _)               i ns es = addChildren i (showNode t) ns es ch1 ch2
    label' t@(Split _ ch1 ch2 _)          i ns es = addChildren i (showNode t) ns es ch1 ch2
    label' t@(Call _ ch _)                i ns es = addChild    i (showNode t) ns es ch

showNode Fail = "_|_"
showNode (Success s)           = "S<BR/>" ++ show s
showNode (Rename id g ts)      = "R " ++ show id ++ "<BR/>" ++ show g ++ "<BR/>" ++ show (reverse ts)
showNode (Gen id g _ curr)     = "G " ++ show id ++ "<BR/>" ++ show g ++ "<BR/>" ++ show curr
showNode (Or _ _ curr)         = "O<BR/>" ++ show curr
showNode (Split id t1 t2 curr) = "Splt " ++ show id ++ "<BR/>" ++ show curr
showNode (Call id t curr)      = "Call " ++ show id ++ "<BR/>" ++ show curr

params :: GraphvizParams n Text Text () Text
params = nonClusteredParams {
  globalAttributes = ga,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
         RankDir FromTop,
         BgColor [toWColor Transparent]
         ],
      NodeAttrs [
        Shape BoxShape,
        FillColor [toWColor White],
        Style [SItem Filled []]
        ]
      ]

    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]

printTree filename tree =
  writeFile filename $ unpack $ renderDot $ toDot $ graphToDot params (treeToGraph tree)
