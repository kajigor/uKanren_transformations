module Printer.Dot where

import           Data.Graph.Inductive              (Gr, mkGraph)
import           Data.GraphViz                     (GlobalAttributes (GraphAttrs, NodeAttrs), GraphvizParams,
                                                    X11Color (Transparent, White), fmtEdge, fmtNode, globalAttributes,
                                                    graphToDot, nonClusteredParams)
import           Data.GraphViz.Attributes.Complete (Attribute (BgColor, FillColor, Label, RankDir, Shape, Style),
                                                    Label (StrLabel), RankDir (FromTop), Shape (BoxShape),
                                                    StyleItem (SItem), StyleName (Filled), toWColor)
import           Data.GraphViz.Printing            (renderDot, toDot)
import           Data.Text.Lazy                    (Text, pack, replace, unpack)

import qualified Data.Set                          as Set
import           Syntax
import           Text.Printf

import Debug.Trace

treeToGraph :: DotPrinter a => a -> Gr Text Text
treeToGraph tree =
  let (vs, es') = label tree in
  let es = Set.toList $ Set.fromList es' in
  mkGraph (map (\(i, v) -> (i, pack (printf "<%s>" v))) vs) (map (\(i,j,l) -> (i,j, pack l)) es)

type Id = Int
type Vertex = (Id, String)
type Edge = (Id, Id, String)

class Dot a => DotPrinter a where
  labelNode :: a -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])

  addLeaf :: a -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])
  addLeaf n nodeId ns es ids = ((nodeId, dot n) : ns, es, ids)

  addChild :: a -> a -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])
  addChild n ch nodeId ns es (childId : ids) =
      let (ns', es', ids') = labelNode ch childId ns es ids
      in  ((nodeId, dot n) : ns', (nodeId, childId, "") : es', ids')

  addChildren :: a -> [a] -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])
  addChildren n ch nodeId ns es ids =
      let (names, otherIds) = splitAt (length ch) ids in
      let (nss, ess, idss) = foldl (\(ns', es', ids') (ch, key) -> labelNode ch key ns' es' ids') (ns, es, otherIds) (zip ch names) in
      ((nodeId, dot n) : nss, map (\x -> (nodeId, x, "")) names ++ ess, idss)

  label :: a -> ([Vertex], [Edge])
  label tree =
    let (treeId : ids) = [0..] in
    let (vs, es, _) = labelNode tree treeId [] [] ids
    in  (vs, es)

  simplify :: a -> a
  simplify = id

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

removeQuots :: Text -> Text
removeQuots =
  replace (pack ">\"") (pack ">") .
  replace (pack "\"<") (pack "<")

simplyPrintTree tree = unpack $ removeQuots $ renderDot $ toDot $ graphToDot params $ treeToGraph tree

printTree :: (Show a, DotPrinter a) => FilePath -> a -> IO ()
printTree filename tree = 
  writeFile filename $ unpack $ removeQuots $ renderDot $ toDot $ graphToDot params $ treeToGraph $ simplify tree
