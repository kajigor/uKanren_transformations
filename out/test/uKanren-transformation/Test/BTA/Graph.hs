module Test.BTA.Graph where

import           Test.Helper
import           BTA.Graph
import           BTA.Conditions
import           BTA.Condition
import           BTA.SizeConversion
import qualified Data.Map as Map


conditions1 = ConditionConj [Condition (Lt "a" "b"), Condition (Eq "c" "d"), Condition (Lt "b" "c")]
vars1 = ["a", "b", "c", "d"]

zeroArc = (Sum 0 Map.empty, WeightedArc)
ltArc = (Sum 1 Map.empty, Arc)
graph1 = Graph vars1 $ Map.fromList [(("b", "a"), ltArc), (("c", "d"), zeroArc), (("d", "c"), zeroArc), (("c", "b"), ltArc)]
graph1_supplement = Graph vars1 $ Map.fromList [(("b", "a"), ltArc), (("c", "d"), zeroArc), (("d", "c"), zeroArc),
 (("c", "b"), ltArc), (("c", "a"), ltArc), (("d", "b"), ltArc), (("d", "a"), ltArc)]
graph1_addedEdgeCA = Graph vars1 $ Map.fromList [(("b", "a"), ltArc), (("c", "d"), zeroArc), (("d", "c"), zeroArc), (("c", "b"), ltArc), (("c", "a"), ltArc)]


conditions2 = ConditionConj [Condition (Lt "a" "b")]
vars2 = ["a", "b"]
graph2 = Graph vars2 $ Map.fromList [(("b", "a"), ltArc)]
graph2_supplement = graph2


conditions3 = ConditionConj [Condition (Lt "c" "a"), Condition (Lt "b" "a"), Condition (Eq "c" "e"), Condition (Lt "f" "e")]
vars3 = ["a", "b", "c", "e", "f"]

unit_graph_from_conditions = do 
    test2 getFromConjunction conditions1 vars1 graph1_supplement
    test2 getFromConjunction conditions2 vars2 graph2_supplement

unit_graph_positive_path = do 
    test3False positivePath "a" "b" graph1
    test3True positivePath "b" "a" graph1
    test3True positivePath "c" "a" graph1
    test3True positivePath "d" "b" graph1

unit_graph_zero_path = do 
    test3True zeroPath "c" "d" graph1


unit_add_edge = do 
    test2 addEdge graph1 ("c", "a") graph1_addedEdgeCA
    test2 addEdge graph1 ("b", "a") graph1
    test2 addEdge graph1 ("a", "c") graph1

unit_graph_supplement = do
    test supplement graph1 graph1_supplement
    test supplement graph2 graph2_supplement

unit_positive_term = do
    testTrue isPositive $ Sum 1 (Map.empty :: Map.Map String Int)
    testFalse isPositive $ Sum 0 (Map.empty :: Map.Map String Int)
    testTrue isPositive $ Sum 0 (Map.fromList [("a", 2)])
    testFalse isPositive $ Sum 3 (Map.fromList [("a", 2), ("b", -1)])


unit_complex_graph = do  
    let graph = getFromConjunction conditions3 vars3
    let graphV = addVertexes ["d"] graph
    let graphVE = addWeightCond graphV "c" "d" $ Sum 1 Map.empty
    test3True positivePath "a" "e" graphVE
    test3False zeroPath "d" "f" graphVE
    test3True zeroPath "e" "c" graphVE
    test3False positivePath "e" "c" graphVE
    test3False positivePath "d" "a" graphVE
    test3True positivePath "e" "d" graphVE