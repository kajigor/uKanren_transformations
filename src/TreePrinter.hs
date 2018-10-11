
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

import Data.List (sort)
import Control.Monad (liftM)
import qualified Eval as E
import Syntax
import Tree
import Text.Printf
import qualified Data.Set as Set

treeToGraph :: Tree -> (Gr Text Text)
treeToGraph tree =
  let (vs, es') = label tree in
  let es = Set.toList $ Set.fromList es' in
  mkGraph (map (\(i, v) -> (i, pack (printf "<%s>" v))) vs) (map (\(i,j,l) -> (i,j, pack l)) es)

label :: Tree -> ([(Int, String)], [(Int, Int, String)])
label tree =
  let (treeId : ids) = [0..] in
  let (vs, es, _) = label' tree treeId [] [] ids
  in  (vs, es)
  where
    label' t@(Call _ ch _ _)  i ns es ids = addChild    t i ns es ids ch
    label' t@(Gen _ _ ch _ _) i ns es ids = addChild    t i ns es ids ch
    label' t@(Or ch1 ch2 _ _) i ns es ids = addChildren t i ns es ids [ch1, ch2]
    label' t@(Split _ ch _ _) i ns es ids = addChildren t i ns es ids ch
    label' t                  i ns es ids = addLeaf     t i ns es ids
    addLeaf n nodeId ns es ids = ((nodeId, dot n) : ns, es, ids)
    addChild n nodeId ns es (childId : ids) ch =
        let (ns', es', ids') = label' ch childId ns es ids
        in  ((nodeId, dot n) : ns', (nodeId, childId, "") : es', ids')
    addChildren n nodeId ns es ids ch =
        let (names, otherIds) = splitAt (length ch) ids in
        let (nss, ess, idss) = foldl (\(ns', es', ids') (ch, key) -> label' ch key ns' es' ids') (ns, es, otherIds) (zip ch names) in
        ((nodeId, dot n) : nss, map (\x -> (nodeId, x, "")) names ++ ess, idss)

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
    let graph = treeToGraph tree
    writeFile filename $ unpack $ remove_quots $ renderDot $ toDot $ graphToDot params graph
