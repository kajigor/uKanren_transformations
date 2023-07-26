
module Test.BTA.TerminationCheck where

import Test.BTA.Inequalities
import Test.Helper
import BTA.TerminationCheck
import Data.Set 
import BTA.Graph
import Data.List
import AnnotatedDef
import qualified InvokeAnnotation as Ann
import AnnotatedProgram
import AnnotationType
import AnnotatedDef
import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe
import BTA.Condition
import BTA.Conditions
import BTA.SizeConversion

mapConds1 = Map.fromList [("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")], ConditionConj [Condition (Lt "y" "z")]]), ("revers", ConditionDisj [ConditionConj []])]
mapDefs1 = (Map.fromList [("append", defAppend), ("revers", defRevers)])

invokeFun = (Invoke "fun" [(Sum 0 (Map.fromList [("a", 1)])), (Sum 0 (Map.fromList [("b", 1)])), (Sum 0 (Map.fromList [("z", 1)]))]) Ann.Unfold
defFun = AnnotatedDef "fun" ["x", "y", "z"] (Fresh "a" (Fresh "b" (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 1 (Map.fromList [("z", 1)]))) []) (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 2 (Map.fromList [("b", 1)]))) ((Sum 0 (Map.fromList [("a", 1)])) :=: (Sum 1 (Map.fromList [("y", 1)]))) [invokeFun]) []))) [Static, Dynamic, Static]
mapDefs2 = (Map.fromList [("fun", defFun)])
mapConds2 = Map.fromList [("fun", ConditionDisj [ConditionConj []])]

mapConds3 = Map.fromList [("append", ConditionDisj [ConditionConj [Condition (Eq "x" "z"),Condition (Eq "z" "x")], ConditionConj [Condition (Lt "x" "z")]]), ("revers", ConditionDisj [ConditionConj []])]
newDefRevers = AnnotatedDef "revers" ["x", "y"] (Fresh "h" (Fresh "t" (Fresh "rt" (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 1 Map.empty)) []) (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 (Map.fromList [("h", 1), ("t", 1)]))) invokeAppend [invokeRevers]) [])))) [Dynamic, Static]
mapDefs3 = (Map.fromList [("append", defAppend), ("revers", newDefRevers)])

unit_getPairsDef = do 
    let (mapPairs, mapVars) = getPairsDef (mapConds1) mapDefs1 defAppend 
    let (vars1, vars2) = fromJust (Map.lookup ("append", "append") mapVars)
    let st = toList $ fromJust (Map.lookup ("append", "append") mapPairs)
    (length st) @?= 1 
    let graph = st !! 0 
    (positivePath (vars1 !! 0) (vars2 !! 0) graph) @?= True
    (positivePath (vars1 !! 2) (vars2 !! 2) graph) @?= True
    
    let (mapPairs1, mapVars1) = getPairsDef (mapConds1) mapDefs1 defRevers 
    let (vars11, vars21) = fromJust (Map.lookup ("revers", "revers") mapVars1)
    let st1 = toList $ fromJust (Map.lookup ("revers", "revers") (traceShow mapPairs1 mapPairs1))
    let graph1 = st1 !! 0 
    (positivePath (vars11 !! 0) (vars21 !! 0) graph1) @?= True
    (positivePath (vars11 !! 1) (vars21 !! 1) graph1) @?= False

    let (mapPairs2, mapVars2) = getPairsDef (mapConds2) mapDefs2 defFun 
    let (vars12, vars22) = fromJust (Map.lookup ("fun", "fun") mapVars2)
    let st1 = toList $ fromJust (Map.lookup ("fun", "fun") (traceShow mapPairs2 mapPairs2))
    let graph2 = st1 !! 0 
    (positivePath (vars12 !! 0) (vars22 !! 1) graph2) @?= True
    (positivePath (vars12 !! 1) (vars22 !! 0) graph2) @?= False
    (zeroPath (vars12 !! 2) (vars22 !! 2) graph2) @?= True
    
    let (mapPairs3, mapVars3) = getPairsDef (mapConds3) mapDefs3 newDefRevers 
    let (vars13, vars23) = fromJust (Map.lookup ("revers", "revers") mapVars3)
    let st3 = toList $ fromJust (Map.lookup ("revers", "revers") (traceShow mapPairs3 mapPairs3))
    let graph3 = st3 !! 1
    (positivePath (vars13 !! 0) (vars23 !! 0) graph3) @?= True
    (positivePath (vars13 !! 1) (vars23 !! 1) graph3) @?= True
    (zeroPath (vars13 !! 1) (vars23 !! 1) graph3) @?= False


unit_goGraphMap = do 
    let (mapPairs, mapVars) = getPairsDef (mapConds1) mapDefs1 defAppend 
    let (mapPairs1, mapVars1) = getPairsDef (mapConds1) mapDefs1 defRevers 
    let mapPairsRes = Map.union mapPairs mapPairs1
    let mapVarsRes = Map.union mapVars mapVars1
    let resGraphMap = goGraphMap mapPairsRes mapVarsRes ["revers", "append"]

    let (vars1, vars2) = fromJust (Map.lookup ("append", "append") mapVarsRes)
    let st = toList $ fromJust (Map.lookup ("append", "append") resGraphMap)
    -- (length st) @?= 1
    let graph = (traceShow st st) !! 0 
    (positivePath (vars1 !! 0) (vars2 !! 0) graph) @?= True
    (positivePath (vars1 !! 2) (vars2 !! 2) graph) @?= True

    let (vars11, vars21) = fromJust (Map.lookup ("revers", "revers") mapVarsRes)
    let st1 = toList $ fromJust (Map.lookup ("revers", "revers") resGraphMap)
    -- (length st1) @?= 1
    let graph = st1 !! 0 
    (positivePath (vars11 !! 0) (vars21 !! 0) graph) @?= True
    -- (positivePath (vars1 !! 2) (vars2 !! 2) graph) @?= True

-- invokeAppendTerm1 = (Invoke "append" [(Sum 0 (fromList [("rt", 1)])), (Sum 2 (fromList [("h", 1)])), (Sum 0 (fromList [("y", 1)]))]) Ann.Unfold
defAppendTerm1 =  AnnotatedDef "append" ["x", "y", "z"] (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 0 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 0 (Map.fromList [("z", 1)]))) []) (Invoke "append" [(Sum (-1) (Map.fromList [("x", 1)])), (Sum (0) (Map.fromList [("y", 1)])), (Sum (-1) (Map.fromList [("z", 1)]))] Ann.Unfold) []) [Dynamic, Static, Dynamic]

invokeAppendMemo = Invoke "append" [(Sum (-1) (Map.fromList [("x", 1)])), (Sum (0) (Map.fromList [("y", 1)])), (Sum (-1) (Map.fromList [("z", 1)]))] Ann.Memo
invokeReversMemo = Invoke "revers" [(Sum 0 (Map.fromList [("t", 1)])), (Sum 0 (Map.fromList [("rt", 1)]))] Ann.Memo
-- invokeAppend = (Invoke "append" [(Sum 0 (fromList [("rt", 1)])), (Sum 2 (fromList [("h", 1)])), (Sum 0 (fromList [("y", 1)]))]) Ann.Unfold
defReversTerm1 = AnnotatedDef "revers" ["x", "y"] (Fresh "h" (Fresh "t" (Fresh "rt" (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 1 Map.empty)) []) (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 (Map.fromList [("h", 1), ("t", 1)]))) invokeReversMemo [invokeAppend]) [])))) [Static, Dynamic]
mapDefTerm1 = (Map.fromList [("append", defAppend), ("revers", defReversTerm1)])
mapDefTerm2 = (Map.fromList [("append", defAppendTerm1), ("revers", defReversTerm1)])

defReversTerm2 = AnnotatedDef "revers" ["x", "y"] (Fresh "h" (Fresh "t" (Fresh "rt" (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 1 Map.empty)) []) (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 (Map.fromList [("h", 1), ("t", 1)]))) invokeRevers [invokeAppend]) [])))) [Dynamic, Static]
defReversTerm3 = AnnotatedDef "revers" ["x", "y"] (Fresh "h" (Fresh "t" (Fresh "rt" (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 1 Map.empty)) []) (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 1 (Map.fromList [("h", 1), ("t", 1)]))) invokeReversMemo [invokeAppend]) [])))) [Dynamic, Static]
mapDefTerm3 = (Map.fromList [("append", defAppend), ("revers", defReversTerm2)])
mapDefTerm4 = (Map.fromList [("append", defAppend), ("revers", defReversTerm3)])

defAppendTerm2 =  AnnotatedDef "append" ["x", "y", "z"] (Disjunction (Conjunction ((Sum 0 (Map.fromList [("x", 1)])) :=: (Sum 0 Map.empty)) ((Sum 0 (Map.fromList [("y", 1)])) :=: (Sum 0 (Map.fromList [("z", 1)]))) []) invokeAppendMemo []) [Dynamic, Static, Dynamic]
mapDefTerm5 = (Map.fromList [("append", defAppendTerm2), ("revers", defReversTerm3)])

unit_terminationCheck = do 
    (terminationCheck (AnnotatedProgram [defAppend, defReversTerm1] invokeRevers) mapConds1 mapDefTerm1) @?= True
    (terminationCheck (AnnotatedProgram [defAppendTerm1] invokeAppend) mapConds1 mapDefTerm2) @?= False
    (terminationCheck (AnnotatedProgram [defAppend, defReversTerm2] invokeRevers) mapConds1 mapDefTerm3) @?= False
    (terminationCheck (AnnotatedProgram [defAppend, defReversTerm3] invokeReversMemo) mapConds1 mapDefTerm4) @?= True
    (terminationCheck (AnnotatedProgram [defAppendTerm2] invokeAppend) mapConds1 mapDefTerm5) @?= True

