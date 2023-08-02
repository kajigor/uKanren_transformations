
module BTA.DotAbstract where 

import Syntax
import BTA.SizeConversion
import BTA.Graph
import qualified Data.Map as Map
import Data.List
import           Util.Miscellaneous                (escapeTick)
import           System.Process                    (system)
import           System.Directory                  (createDirectoryIfMissing)
import           Text.Printf
import           Data.Text.Lazy                    (Text, pack, replace, unpack)
import           Data.Graph.Inductive              (Gr, mkGraph)
import           Data.GraphViz.Printing            (renderDot, toDot)
import           Data.GraphViz                     (GlobalAttributes (GraphAttrs, NodeAttrs), GraphvizParams,
                                                    X11Color (Transparent, White), fmtEdge, fmtNode, globalAttributes,
                                                    graphToDot, nonClusteredParams)
import           Data.GraphViz.Attributes.Complete (Attribute (BgColor, FillColor, Label, RankDir, Shape, Style),
                                                    Label (StrLabel), RankDir (FromTop), Shape (BoxShape),
                                                    StyleItem (SItem), StyleName (Filled), toWColor)
import System.FilePath ((<.>), (</>))
import GHC.IO.Exception (ExitCode)

instance Dot TypeEdge where 
    dot = show 

instance (Dot a, Ord a) => Dot (AbstractTerm a) where 
    dot (Sum n mp) | mp == Map.empty = dot n
    dot (Sum n mp) = dot n ++ " + " ++ (intercalate " + " $ map (\(key, val) -> dot val ++ "*" ++ dot key) $ Map.toList mp)

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


treeToGraph :: Graph String -> Gr Text Text
treeToGraph (Graph vars edgesMap) =
  let vs = zip [0..] $ map dot vars in 
  let vs1 = Map.fromList $ zip vars [0..] in 
  let es = map (\((a, b), (w, t)) -> (vs1 Map.! a, vs1 Map.! b, w)) $ Map.assocs edgesMap in 
  mkGraph (map (\(i, v) -> (i, pack (printf "<%s>" v))) vs) (map (\(i,j,l) -> (i,j, pack $ dot l)) es)


removeQuots :: Text -> Text
removeQuots =
  replace (pack ">\"") (pack ">") .
  replace (pack "\"<") (pack "<")


printTree :: FilePath -> FilePath -> Graph String -> IO ExitCode
printTree dir fileName graph = do 
  createDirectoryIfMissing True dir
  writeFile (dir </> fileName) $ unpack $ removeQuots $ renderDot $ toDot $ graphToDot params $ treeToGraph graph
  system (printf "dot -O -Tpdf %s" (escapeTick (dir </> fileName)))