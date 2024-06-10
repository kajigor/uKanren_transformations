
module Test.BTA.Inequalities where

import           Control.Monad.State
import           Data.Group
import           Data.Map
import           FreshNames
import           Test.Helper
import           BTA.AnnotatedDef 
import           BTA.AnnotationType
import           BTA.Condition
import           BTA.Conditions
import           BTA.Graph
import           BTA.Inequalities
import           BTA.InvokeAnnotation 
import           BTA.SizeConversion
import           Test.BTA.Conditions
import           Test.BTA.AbstractProgram


xyzVars = ["x", "y", "z"]
x1x2x3Vars = ["x1", "x2", "x3"]
xyzx'Vars = ["x", "y", "z", "x'"]
xyzx'y'Vars = ["x", "y", "z", "x'", "y'"]
xyzMap = fromList $ zip xyzVars xyzVars

oneTerm = Sum 1 empty
zeroTerm = Sum 0 empty
a1Term = Sum 0 (fromList [("a", 1)])
xTerm = Sum 0 (fromList [("x", 1)])
yTerm = Sum 0 (fromList [("y", 1)])
zTerm = Sum 0 (fromList [("z", 1)])

unit_getCondsOneTerm = do 
    test getCondsOneTerm ("a", Sum 2 (fromList [("b", 2)])) [("a", "b", Sum 2 (fromList [("b", 1)]))]
    test getCondsOneTerm ("a", Sum 2 (fromList [("a", 2)])) []
    test getCondsOneTerm ("a", Sum 2 (fromList [("c", 2), ("b", 2)])) [("a", "b", (Sum 2 (fromList [("b", 1), ("c", 2)]))), ("a", "c", (Sum 2 (fromList [("b", 2), ("c", 1)])))]

invoke1Terms = [Sum 1 (fromList [("x", 1), ("y", 1)]), xTerm, Sum 1 (fromList [("z", 2)])]
invoke1Graph = Graph xyzVars (fromList [(("x", "z"), (oneTerm, Arc))])
invoke1Conditions = ConditionConj [Condition (Eq "y1" "z1"), Condition (Eq "z1" "y1"), Condition (Lt "y1" "x1")]

invoke2Terms = [Sum (-1) (fromList [("x", 1)]), yTerm, Sum (-1) (fromList [("z", 1)])]
invoke2Graph = Graph xyzVars (fromList [(("x", "z"), (oneTerm, Arc))])
invoke2Conditions = ConditionConj [Condition (Lt "z1" "x1")]

term3Graph = Sum 1 (fromList [("x", -1), ("a", 2)])
invoke3Terms = [a1Term, Sum (-1) (fromList [("a", -1), ("x", 1)])]
invoke3Graph = Graph ["x", "a", "x1", "a1"] $ fromList [
    (("x1", "a"), (zeroTerm, WeightedArc))
    , (("a", "x1"), (zeroTerm, WeightedArc))
    , (("x", "a1"), (sum_a_1, WeightedArc))
    , (("a1", "x"), (invert sum_a_1, WeightedArc))
    , (("x", "x1"), (oneTerm, Arc))
    , (("x", "a"), (oneTerm, Arc))
    , (("a1", "x1"), (oneTerm, Arc))
    , (("a", "a1"), (term3Graph, WeightedArc))
    , (("a1", "a"), (invert term3Graph, WeightedArc))
    ]

invoke3Conditions = ConditionConj [(Condition (Lt "x1" "a1"))]

unit_handleOneInvokeConjunct = do
    test2 cleanGraph (handleOneInvokeConjunct invoke1Terms ["x1", "y1", "z1"] xyzVars invoke1Conditions) xyzVars invoke1Graph
    test2 cleanGraph (handleOneInvokeConjunct invoke2Terms ["x1", "y1", "z1"] xyzVars invoke2Conditions) xyzVars invoke2Graph
    test4 handleOneInvokeConjunct invoke3Terms ["x1", "a1"] ["x", "a"] invoke3Conditions invoke3Graph

oneConjunct1Graph = Graph x1x2x3Vars (fromList [(("x1", "x3"), (Sum 3 empty, WeightedArc)), (("x3", "x1"), (Sum (-3) empty, WeightedArc))])
oneConjunct1MapVars = fromList [("x", "x1"), ("a", "x2"), ("y", "x3")] 

oneConjunct2MapDefs = fromList [("fun", AnnotatedDef "fun" xyzVars (zeroTerm :=: oneTerm) [Dynamic, Dynamic, Dynamic])] 
oneConjunct2MapConditions = fromList [("fun", ConditionDisj [ConditionConj [Condition (Eq "y" "z"), Condition (Eq "z" "y"), Condition (Lt "y" "x")]])]

oneConjunct3MapConditions = fromList [("fun", ConditionDisj [ConditionConj [Condition (Lt "z" "x")]])]

oneConjunct4MapDefs = fromList [("fun", AnnotatedDef "fun" ["x", "a"] (zeroTerm :=: oneTerm) [Dynamic, Dynamic, Dynamic])] 
oneConjunct4MapConditions = fromList [("fun", ConditionDisj [ConditionConj [Condition (Lt "x" "a")]])]
oneConjunct4Graph = Graph ["x", "a", "0", "1"] $ fromList [
    (("0", "a"), (zeroTerm, WeightedArc))
    , (("a", "0"), (zeroTerm, WeightedArc))
    , (("x", "1"), (sum_a_1, WeightedArc))
    , (("1", "x"), (invert sum_a_1, WeightedArc))
    , (("x", "0"), (oneTerm, Arc))
    , (("x", "a"), (oneTerm, Arc))
    , (("1", "0"), (oneTerm, Arc))
    , (("a", "1"), (term3Graph, WeightedArc))
    , (("1", "a"), (invert term3Graph, WeightedArc))
    ]

oneConjunct1 = Sum 1 (fromList [("x", 1), ("a", 1)]) :=: Sum 4 (fromList [("a", 1), ("y", 1)])
oneConjunct2 = Invoke "fun" invoke1Terms Unfold
oneConjunct3 = Invoke "fun" invoke2Terms Unfold
oneConjunct4 = Invoke "fun" invoke3Terms Unfold

unit_goOneConjunct = do 
    (fst $ fst $ runState (goOneConjunct oneConjunct1 empty empty x1x2x3Vars oneConjunct1MapVars) defaultNames) @?= [oneConjunct1Graph]
    cleanGraph (fst (fst $ runState (goOneConjunct oneConjunct2 oneConjunct2MapDefs oneConjunct2MapConditions xyzVars xyzMap) defaultNames) !! 0) xyzVars @?= invoke1Graph
    cleanGraph (fst (fst $ runState (goOneConjunct oneConjunct3 oneConjunct2MapDefs oneConjunct3MapConditions xyzVars xyzMap) defaultNames) !! 0) xyzVars @?= invoke2Graph
    (fst (fst $ runState (goOneConjunct oneConjunct4 oneConjunct4MapDefs oneConjunct4MapConditions ["x", "a"] (fromList [("x", "x"), ("a", "a")])) defaultNames) !! 0) @?= oneConjunct4Graph

goBody1MapConditions = fromList [
    ("fun", ConditionDisj [ConditionConj [Condition (Eq "y" "z"), Condition (Eq "z" "y"), Condition (Lt "y" "x")]])
    , ("fun1", ConditionDisj [ConditionConj [Condition (Lt "z" "x")]])
    ]

goBody1MapDefs = fromList [
    ("fun", AnnotatedDef "fun" xyzVars (zeroTerm :=: oneTerm) [Dynamic, Dynamic, Dynamic])
    , ("fun1", AnnotatedDef "fun1" xyzVars (zeroTerm :=: oneTerm) [Dynamic, Dynamic, Dynamic])
    ]
 
goBody1Graph = Graph xyzVars (fromList [(("x", "z"), (oneTerm, Arc))])
goBody1MapConditionsRes = ConditionDisj [ConditionConj [Condition (Lt "z" "x")]]

invokeBodyTerms = [Sum 0 (fromList [("x'", 1)]), xTerm, Sum 1 (fromList [("z", 2)])]
goBody2MapVars = fromList [("x", "x"), ("y", "y"), ("z", "z"), ("x'", "x'")]
goBody2MapConditionsRes = ConditionDisj [ConditionConj [(Condition (Lt "x" "x'")), (Condition (Lt "z" "x'")), (Condition (Lt "y" "x'"))]] 

invokeBody2Terms = [Sum 0 (fromList [("x'", 1)]), xTerm, Sum 0 (fromList [("y'", 1)])]
goBody3MapVars = fromList [("x", "x"), ("y", "y"), ("z", "z"), ("x'", "x'"), ("y'", "y'")]
goBody3MapConditionsRes = ConditionDisj [ConditionConj [Condition (Lt "z" "y'"), Condition (Lt "y'" "x'"), Condition (Lt "x" "x'"), Condition (Lt "z" "x'"), Condition (Lt "y" "x'")]]


goBody1 = Conjunction (
        oneConjunct2
    ) (
        Invoke "fun1" invoke2Terms Unfold
    ) []

goBody2 = Conjunction (
        Sum 0 (fromList [("x'", 1)]) :=: Sum 1 (fromList [("x", 1), ("y", 1)])
    ) (
        Invoke "fun1" invokeBodyTerms Unfold
    ) []

goBody3 = Conjunction (
        Sum 0 (fromList [("x'", 1)]) :=: Sum 1 (fromList [("x", 1), ("y", 1)])
    ) (
        Invoke "fun1" invokeBody2Terms Unfold
    ) [Sum 0 (fromList [("y'", 1)]) :=: Sum 1 (fromList [("z", 2)])]

goBody4 = Fresh "x'" $ Fresh "y'" $ 
    Conjunction (
        Sum 0 (fromList [("x'", 1)]) :=: Sum 1 (fromList [("x", 1), ("y", 1)])
    ) (
        Invoke "fun" invokeBody2Terms Unfold
    ) [Sum 0 (fromList [("y'", 1)]) :=: Sum 1 (fromList [("z", 2)])]

unit_goBody = do 
    test7 goBody goBody1 goBody1MapDefs goBody1MapConditions xyzVars xyzVars defaultNames xyzMap goBody1MapConditionsRes
    test7 goBody goBody2 goBody1MapDefs goBody1MapConditions xyzx'Vars xyzx'Vars defaultNames goBody2MapVars goBody2MapConditionsRes
    test7 goBody goBody3 goBody1MapDefs goBody1MapConditions xyzx'y'Vars xyzx'y'Vars defaultNames goBody3MapVars goBody3MapConditionsRes
    test7 goBody goBody4 goBody1MapDefs goBody1MapConditions xyzVars xyzVars defaultNames xyzMap goBody1MapConditionsRes
    test7 goBody goBody4 goBody1MapDefs empty xyzVars xyzVars defaultNames xyzMap disjEmpty
    test7 goBody oneConjunct2 goBody1MapDefs goBody1MapConditions xyzVars xyzVars defaultNames xyzMap goBody1MapConditionsRes

defAppend = AnnotatedDef "append" xyzVars (
        Disjunction (
            Conjunction (
                xTerm :=: zeroTerm
            ) (
                yTerm :=: zTerm
            ) []
        ) (
            Invoke "append" [Sum (-1) (fromList [("x", 1)]), yTerm, Sum (-1) (fromList [("z", 1)])] Unfold
        ) []
    ) [Dynamic, Static, Static]

condsResAppend = ConditionDisj [ConditionConj [Condition (Eq "y" "z")]]
resMapAppend1 = fromList [("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")]])]

invokeRevers = Invoke "revers" [Sum 0 (fromList [("t", 1)]), Sum 0 (fromList [("rt", 1)])] Unfold
invokeAppend = Invoke "append" [Sum 0 (fromList [("rt", 1)]), Sum 2 (fromList [("h", 1)]), yTerm] Unfold
defRevers = AnnotatedDef "revers" ["x", "y"] (
    Fresh "h" $ Fresh "t" $ Fresh "rt" $ 
        Disjunction (
            Conjunction (
                xTerm :=: oneTerm
            ) (
                yTerm :=: oneTerm
            ) []
        ) (
            Conjunction (
                xTerm :=: Sum 1 (fromList [("h", 1), ("t", 1)])
            ) (
                invokeRevers
            ) [
                invokeAppend
            ]
        ) []
    ) [Dynamic, Static]

resMapAppRev1 = fromList [("revers", ConditionDisj [ConditionConj [Condition (Eq "x" "y"), Condition (Eq "y" "x")]])]
resMapAppRev2 = fromList [
    ("revers", ConditionDisj [ConditionConj [Condition (Eq "x" "y"), Condition (Eq "y" "x")]])
    , ("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")]])
    ]

oneCycleMap = fromList [("append", defAppend), ("revers", defRevers)]

unit_goOneCycle = do 
    test3 goOneDef defAppend oneCycleMap empty resMapAppend1
    test3 goOneCycle [defAppend] oneCycleMap empty resMapAppend1
    test3 goOneDef defRevers oneCycleMap empty resMapAppRev1
    test3 goOneCycle [defRevers] oneCycleMap empty resMapAppRev1
    test3 goOneCycle [defRevers, defAppend] oneCycleMap empty resMapAppRev2

resMapGo1 = fromList [("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")], ConditionConj [Condition (Lt "y" "z")]])]
resMapGo2 = fromList [
    ("append", ConditionDisj [ConditionConj [Condition (Eq "y" "z"),Condition (Eq "z" "y")], ConditionConj [Condition (Lt "y" "z")]])
    , ("revers", ConditionDisj [ConditionConj []])
    ]

unit_go = do 
    test3 go [defAppend] oneCycleMap empty resMapGo1
    test3 go [defRevers] oneCycleMap resMapGo1 resMapGo2
    test3 go [defRevers, defAppend] oneCycleMap empty resMapGo2
