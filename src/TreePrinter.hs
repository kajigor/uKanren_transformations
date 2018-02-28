
module TreePrinter where 

import Data.Text.Lazy (Text, pack, unpack, replace)
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
import System.Mem.StableName 

treeToGraph :: Tree -> IO (Gr Text Text)
treeToGraph tree = 
  do 
    (vs, es) <- label tree 
    return $ mkGraph (map (\(i, v) -> (i, pack ("<" ++ v ++ ">"))) vs) (map (\(i,j,l) -> (i,j, pack l)) es)

label :: Tree -> IO ([(Int, String)], [(Int, Int, String)])
label tree = 
  do 
    treeId <- makeId tree
    label' tree treeId [] [] 
    where
      label' t@(Call _ ch _ _)       i ns es = addChild    t i ns es ch
      label' t@(Gen _ _ ch _ _)      i ns es = addChild    t i ns es ch
      label' t@(Or ch1 ch2 _ _)      i ns es = addChildren t i ns es ch1 ch2
      label' t@(Split _ ch1 ch2 _ _) i ns es = addChildren t i ns es ch1 ch2
      label' t                       i ns es = addLeaf     t i ns es
      addLeaf n nodeId ns es = return ((nodeId, dot n) : ns, es)
      addChild n nodeId ns es ch = 
        do 
          childId <- makeId ch 
          (ns', es') <- label' ch childId ns es 
          return ((nodeId, dot n) : ns', (nodeId, childId, "") : es')
      addChildren n nodeId ns es ch1 ch2 = 
        do 
          cId1 <- makeId ch1
          cId2 <- makeId ch2
          let (childId1, childId2) = if cId1 > cId2 then (cId2, cId1) else (cId1, cId2)
          (ns',  es')  <- label' ch1 childId1 ns es
          (ns'', es'') <- label' ch2 childId2 ns es
          return((nodeId, dot n) : (ns' ++ ns''), (nodeId, childId1, "") : (nodeId, childId2, "") : (es' ++ es''))
      makeId t = 
        do 
          stable <- makeStableName t 
          return (hashStableName stable)

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

remove_quots t = 
  replace (pack ">\"") (pack ">") $
  replace (pack "\"<") (pack "<") t

printTree filename tree = 
  do
    graph <- treeToGraph tree 
    writeFile filename $ unpack $ remove_quots $ renderDot $ toDot $ graphToDot params graph
