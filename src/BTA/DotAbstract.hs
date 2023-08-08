
module BTA.DotAbstract where 

import           Data.List
import           Data.Graph.Inductive              (Gr, mkGraph)
import           Data.GraphViz.Printing            (renderDot, toDot)
import           Data.GraphViz                     (graphToDot)
import           Data.Text.Lazy                    (Text, pack, unpack)
import           GHC.IO.Exception                  (ExitCode)
import           System.Directory                  (createDirectoryIfMissing)
import           System.FilePath                   ((</>))
import           System.Process                    (system)
import           Text.Printf
import           BTA.SizeConversion
import           BTA.Graph
import           Printer.Dot                       (params, removeQuots)
import           Syntax
import           Util.Miscellaneous                (escapeTick)
import qualified Data.Map                           as Map

instance Dot TypeEdge where 
    dot = show 

instance (Dot a, Ord a) => Dot (AbstractTerm a) where 
    dot (Sum n mp) | mp == Map.empty = dot n
    dot (Sum n mp) = dot n ++ " + " ++ (intercalate " + " $ map (\(key, val) -> dot val ++ "*" ++ dot key) $ Map.toList mp)

treeToGraph :: Graph String -> Gr Text Text
treeToGraph (Graph vars edgesMap) =
  let vs = zip [0..] $ map dot vars in 
  let vs1 = Map.fromList $ zip vars [0..] in 
  let es = map (\((a, b), (w, t)) -> (vs1 Map.! a, vs1 Map.! b, w)) $ Map.assocs edgesMap in 
  mkGraph (map (\(i, v) -> (i, pack (printf "<%s>" v))) vs) (map (\(i,j,l) -> (i,j, pack $ dot l)) es)

printTree :: FilePath -> FilePath -> Graph String -> IO ExitCode
printTree dir fileName graph = do 
  createDirectoryIfMissing True dir
  writeFile (dir </> fileName) $ unpack $ removeQuots $ renderDot $ toDot $ graphToDot params $ treeToGraph graph
  system (printf "dot -O -Tpdf %s" (escapeTick (dir </> fileName)))