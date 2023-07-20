module Test.BTA.Graph where

import Test.Helper
import BTA.Graph
import BTA.Conditions
import BTA.Condition
import BTA.SizeConversion
import qualified Data.Map as Map


conditions1 = (ConditionConj [(Condition (Lt "a" "b")), (Condition (Eq "c" "d")), (Condition (Lt "b" "c"))])
vars1 = ["a", "b", "c", "d"]
graph1 = (Graph vars1 (Map.fromList [(("b", "a"), (Sum 1 Map.empty, Arc)), (("c", "d"), (Sum 0 Map.empty, WeightedArc)), (("d", "c"), (Sum 0 Map.empty, WeightedArc)), (("c", "b"), (Sum 1 Map.empty, Arc))]))
graph1_supplement = (Graph vars1 (Map.fromList [(("b", "a"), (Sum 1 Map.empty, Arc)), (("c", "d"), (Sum 0 Map.empty, WeightedArc)), (("d", "c"), (Sum 0 Map.empty, WeightedArc)),
 (("c", "b"), (Sum 1 Map.empty, Arc)), (("c", "a"), (Sum 1 Map.empty, Arc)), (("d", "b"), (Sum 1 Map.empty, Arc)), (("d", "a"), (Sum 1 Map.empty, Arc))]))
graph1_addedEdgeCA = (Graph vars1 (Map.fromList [(("b", "a"), (Sum 1 Map.empty, Arc)), (("c", "d"), (Sum 0 Map.empty, WeightedArc)), (("d", "c"), (Sum 0 Map.empty, WeightedArc)), (("c", "b"), (Sum 1 Map.empty, Arc)), (("c", "a"), (Sum 1 Map.empty, Arc))]))


conditions2 = (ConditionConj [(Condition (Lt "a" "b"))])
vars2 = ["a", "b"]
graph2 = (Graph vars2 (Map.fromList [(("b", "a"), (Sum 1 Map.empty, Arc))]))
graph2_supplement = graph2


conditions3 = (ConditionConj [(Condition (Lt "c" "a")), (Condition (Lt "b" "a")), (Condition (Eq "c" "e")), (Condition (Lt "f" "e"))])
vars3 = ["a", "b", "c", "e", "f"]

unit_graph_from_conditions = do 
    (getFromConjunction conditions1 vars1) @?= graph1_supplement
    (getFromConjunction conditions2 vars2) @?= graph2_supplement

unit_graph_positive_path = do 
    (positivePath "a" "b" graph1) @?= False
    (positivePath "b" "a" graph1) @?= True
    (positivePath "c" "a" graph1) @?= True
    (positivePath "d" "b" graph1) @?= True

unit_graph_zero_path = do 
    (zeroPath "c" "d" graph1) @?= True


unit_add_edge = do 
    (addEdge graph1 ("c", "a")) @?= graph1_addedEdgeCA
    (addEdge graph1 ("b", "a")) @?= graph1
    (addEdge graph1 ("a", "c")) @?= graph1

unit_graph_supplement = do
    (supplement graph1) @?= graph1_supplement
    (supplement graph2) @?= graph2_supplement

unit_positive_term = do
    (isPositive (Sum 1 (Map.empty :: Map.Map String Int))) @?= True
    (isPositive (Sum 0 (Map.empty :: Map.Map String Int))) @?= False
    (isPositive (Sum 0 (Map.fromList [("a", 2)]))) @?= True
    (isPositive (Sum 3 (Map.fromList [("a", 2), ("b", -1)]))) @?= False


unit_complex_graph = do  
    let graph = (getFromConjunction conditions3 vars3)
    let graphV = (addVertex graph "d")
    let graphVE = (addWeightCond graphV "c" "d" (Sum 1 Map.empty))
    (positivePath "a" "e" graphVE) @?= True
    (zeroPath "d" "f" graphVE) @?= False
    (zeroPath "e" "c" graphVE) @?= True
    (positivePath "e" "c" graphVE) @?= False
    (positivePath "d" "a" graphVE) @?= False
    (positivePath "e" "d" graphVE) @?= True