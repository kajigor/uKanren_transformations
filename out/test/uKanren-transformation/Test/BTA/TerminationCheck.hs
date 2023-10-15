
module Test.BTA.TerminationCheck where

import           Data.Set 
import           Debug.Trace
import           Test.Helper
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.AnnotationType
import           BTA.Condition
import           BTA.Conditions
import           BTA.Graph
import           BTA.InvokeAnnotation
import           BTA.SizeConversion
import           BTA.TerminationCheck
import           Test.BTA.Conditions
import           Test.BTA.Inequalities
import qualified Data.Map as Map

reverseBase = Conjunction (
        xTerm :=: oneTerm
    ) (
        yTerm :=: oneTerm
    ) []

mapConds1 = Map.fromList [
    ("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")], ConditionConj [Condition (Lt "y" "z")]])
    , ("revers", disjEmpty)
    ]

mapDefs1 = Map.fromList [("append", defAppend), ("revers", defRevers)]

invokeFun = Invoke "fun" [Sum 0 (Map.fromList [("a", 1)]), Sum 0 (Map.fromList [("b", 1)]), zTerm] Unfold

defFun = AnnotatedDef "fun" ["x", "y", "z"] (
    Fresh "a" $ Fresh "b" $ 
        Disjunction (
            Conjunction (
                xTerm :=: oneTerm
            ) (
                yTerm :=: Sum 1 (Map.fromList [("z", 1)])
            ) []
        ) (
            Conjunction (
                xTerm :=: Sum 2 (Map.fromList [("b", 1)])
            ) (
                Sum 0 (Map.fromList [("a", 1)]) :=: Sum 1 (Map.fromList [("y", 1)])
            ) [invokeFun]
        ) []
    ) [Static, Dynamic, Static]

mapDefs2 = Map.fromList [("fun", defFun)]
mapConds2 = Map.fromList [("fun", disjEmpty)]
mapConds3 = Map.fromList [
    ("append", ConditionDisj [ConditionConj [Condition (Eq "x" "z"),Condition (Eq "z" "x")], ConditionConj [Condition (Lt "x" "z")]])
    , ("revers", disjEmpty)
    ]

newDefRevers = AnnotatedDef "revers" ["x", "y"] (
    Fresh "h" $ Fresh "t" $ Fresh "rt" $ 
        Disjunction (
            reverseBase
        ) (
            Conjunction (
                xTerm :=: Sum 1 (Map.fromList [("h", 1), ("t", 1)])
            ) (
                invokeAppend
            ) [invokeRevers]
        ) []
    ) [Dynamic, Static]

mapDefs3 = Map.fromList [("append", defAppend), ("revers", newDefRevers)]

unit_getPairsDef = do 
    let (mapPairs, mapVars) = getPairsDef mapConds1 mapDefs1 defAppend 
    let (vars1, vars2) = mapVars Map.! ("append", "append")
    let st = toList $ mapPairs Map.! ("append", "append")
    length st @?= 1 
    let graph = st !! 0 
    test3True positivePath (vars1 !! 0) (vars2 !! 0) graph
    test3True positivePath (vars1 !! 2) (vars2 !! 2) graph
    
    let (mapPairs1, mapVars1) = getPairsDef mapConds1 mapDefs1 defRevers 
    let (vars11, vars21) = mapVars1 Map.! ("revers", "revers")
    let st1 = toList $ mapPairs1 Map.! ("revers", "revers")
    let graph1 = st1 !! 0 
    test3True positivePath (vars11 !! 0) (vars21 !! 0) graph1
    test3False positivePath (vars11 !! 1) (vars21 !! 1) graph1

    let (mapPairs2, mapVars2) = getPairsDef mapConds2 mapDefs2 defFun 
    let (vars12, vars22) = mapVars2 Map.! ("fun", "fun")
    let st1 = toList $ mapPairs2 Map.! ("fun", "fun")
    let graph2 = st1 !! 0 
    test3True positivePath (vars12 !! 0) (vars22 !! 1) graph2
    test3False positivePath (vars12 !! 1) (vars22 !! 0) graph2
    test3True zeroPath (vars12 !! 2) (vars22 !! 2) graph2
    
    let (mapPairs3, mapVars3) = getPairsDef mapConds3 mapDefs3 newDefRevers 
    let (vars13, vars23) = mapVars3 Map.! ("revers", "revers")
    let st3 = toList $ mapPairs3 Map.! ("revers", "revers")
    let graph3 = st3 !! 1
    test3True positivePath (vars13 !! 0) (vars23 !! 0) graph3
    test3True positivePath (vars13 !! 1) (vars23 !! 1) graph3
    test3False zeroPath (vars13 !! 1) (vars23 !! 1) graph3


unit_goGraphMap = do 
    let (mapPairs, mapVars) = getPairsDef mapConds1 mapDefs1 defAppend 
    let (mapPairs1, mapVars1) = getPairsDef mapConds1 mapDefs1 defRevers 
    let mapPairsRes = Map.union mapPairs mapPairs1
    let mapVarsRes = Map.union mapVars mapVars1
    let resGraphMap = goGraphMap mapPairsRes mapVarsRes ["revers", "append"]

    let (vars1, vars2) = mapVarsRes Map.! ("append", "append")
    let st = toList $ resGraphMap Map.! ("append", "append")
    let graph = st !! 0 
    positivePath (vars1 !! 0) (vars2 !! 0) graph @?= True
    positivePath (vars1 !! 2) (vars2 !! 2) graph @?= True

    let (vars11, vars21) = mapVarsRes Map.! ("revers", "revers")
    let st1 = toList $ resGraphMap Map.! ("revers", "revers")
    let graph = st1 !! 0 
    positivePath (vars11 !! 0) (vars21 !! 0) graph @?= True

defAppendTerm1 =  AnnotatedDef "append" ["x", "y", "z"] (
    Disjunction (
        Conjunction (
            xTerm :=: zeroTerm
        ) (
            yTerm :=: zTerm
        ) []
    ) (
        Invoke "append" [Sum (-1) (Map.fromList [("x", 1)]), yTerm, Sum (-1) (Map.fromList [("z", 1)])] Unfold
    ) []
    ) [Dynamic, Static, Dynamic]

invokeAppendMemo = Invoke "append" [Sum (-1) (Map.fromList [("x", 1)]), yTerm, Sum (-1) (Map.fromList [("z", 1)])] Memo
invokeReversMemo = Invoke "revers" [Sum 0 (Map.fromList [("t", 1)]), Sum 0 (Map.fromList [("rt", 1)])] Memo

defReversTerm1 = AnnotatedDef "revers" ["x", "y"] (
    Fresh "h" $ Fresh "t" $ Fresh "rt" $ 
        Disjunction (
            reverseBase
        ) (
            Conjunction (
                xTerm :=: Sum 1 (Map.fromList [("h", 1), ("t", 1)])
            ) (
                invokeReversMemo
            ) [invokeAppend]
        ) []
    ) [Static, Dynamic]

mapDefTerm1 = Map.fromList [("append", defAppend), ("revers", defReversTerm1)]
mapDefTerm2 = Map.fromList [("append", defAppendTerm1), ("revers", defReversTerm1)]

defReversTerm2 = AnnotatedDef "revers" ["x", "y"] (
    Fresh "h" $ Fresh "t" $ Fresh "rt" $ 
        Disjunction (
            reverseBase
        ) (
            Conjunction (
                xTerm :=: Sum 1 (Map.fromList [("h", 1), ("t", 1)])
            ) (
                invokeRevers
            ) [invokeAppend]
        ) []
    ) [Dynamic, Static]

defReversTerm3 = AnnotatedDef "revers" ["x", "y"] (
    Fresh "h" $ Fresh "t" $ Fresh "rt" $ 
        Disjunction (
            reverseBase
        ) (
            Conjunction (
                xTerm :=: (Sum 1 (Map.fromList [("h", 1), ("t", 1)]))
            ) (
                invokeReversMemo
            ) [invokeAppend]
        ) []
    ) [Dynamic, Static]

mapDefTerm3 = Map.fromList [("append", defAppend), ("revers", defReversTerm2)]
mapDefTerm4 = Map.fromList [("append", defAppend), ("revers", defReversTerm3)]

defAppendTerm2 =  AnnotatedDef "append" ["x", "y", "z"] (
    Disjunction (
        Conjunction (
            xTerm :=: zeroTerm
        ) (
            yTerm :=: zTerm
        ) []
    ) (
        invokeAppendMemo
    ) []
    ) [Dynamic, Static, Dynamic]

mapDefTerm5 = Map.fromList [("append", defAppendTerm2), ("revers", defReversTerm3)]

unit_terminationCheck = do 
    test3True terminationCheck (AnnotatedProgram [defAppend, defReversTerm1] invokeRevers) mapConds1 mapDefTerm1
    test3False terminationCheck (AnnotatedProgram [defAppendTerm1] invokeAppend) mapConds1 mapDefTerm2
    test3False terminationCheck (AnnotatedProgram [defAppend, defReversTerm2] invokeRevers) mapConds1 mapDefTerm3
    test3True terminationCheck (AnnotatedProgram [defAppend, defReversTerm3] invokeReversMemo) mapConds1 mapDefTerm4
    test3True terminationCheck (AnnotatedProgram [defAppendTerm2] invokeAppend) mapConds1 mapDefTerm5

