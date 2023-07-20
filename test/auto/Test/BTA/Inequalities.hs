
module Test.BTA.Inequalities where

import Test.Helper
import BTA.Inequalities
import BTA.SizeConversion
import BTA.Conditions
import BTA.Condition
import BTA.Graph
import Data.Map
import FreshNames
import AnnotatedDef 
import AnnotationType
import qualified InvokeAnnotation as Ann
import Debug.Trace
import Control.Monad.State


unit_getCondsFromEq = do
    length (getCondsFromEq (Sum 0 (fromList [("a", 1)])) (Sum 0 (fromList [("b", 1)]))) @?= 1
    (equalConds ((getCondsFromEq (Sum 0 (fromList [("a", 1)])) (Sum 0 (fromList [("b", 1)]))) !! 0) (Condition (Eq "a" "b"))) @?= True
    (equalConds (ConditionConj (getCondsFromEq (Sum 0 (fromList [("a", 1)])) (Sum 0 (fromList [("b", 1), ("c", 1)])))) (ConditionConj [(Condition (Lt "b" "a")), (Condition (Lt "c" "a"))] )) @?= True
    (equalConds (ConditionConj (getCondsFromEq (Sum 0 (fromList [("a", 2)])) (Sum 0 (fromList [("b", 1), ("c", 1)])))) (ConditionConj [] )) @?= True
    (equalConds (ConditionConj (getCondsFromEq (Sum 0 (fromList [("a", 2)])) (Sum 0 (fromList [("b", 1), ("c", 1)])))) (ConditionConj [(Condition (Lt "b" "a")), (Condition (Lt "c" "a"))] )) @?= False
    (equalConds (ConditionConj (getCondsFromEq (Sum 0 (fromList [("a", 1), ("b", 1)])) (Sum 0 (fromList [("b", 1), ("c", 1)])))) (ConditionConj [(Condition (Eq "a" "c"))] )) @?= True


unit_getCondsOneTerm = do 
    (getCondsOneTerm ("a", (Sum 2 (fromList [("b", 2)])))) @?= [("a", "b", (Sum 2 (fromList [("b", 1)])))]
    (getCondsOneTerm ("a", (Sum 2 (fromList [("a", 2)])))) @?= []
    (getCondsOneTerm ("a", (Sum 2 (fromList [("c", 2), ("b", 2)])))) @?= [("a", "b", (Sum 2 (fromList [("b", 1), ("c", 2)]))), ("a", "c", (Sum 2 (fromList [("b", 2), ("c", 1)])))]


invoke1Terms = [(Sum 1 (fromList [("x", 1), ("y", 1)])), (Sum 0 (fromList [("x", 1)])), (Sum 1 (fromList [("z", 2)]))]
invoke1Graph = (Graph ["x", "y", "z"] (fromList [(("x", "z"), (Sum 1 empty, Arc))]))
invoke1Conditions = ConditionConj [(Condition (Eq "y1" "z1")), (Condition (Eq "z1" "y1")), (Condition (Lt "y1" "x1"))]

invoke2Terms = [(Sum (-1) (fromList [("x", 1)])), (Sum 0 (fromList [("y", 1)])), (Sum (-1) (fromList [("z", 1)]))]
invoke2Graph = (Graph ["x", "y", "z"] (fromList [(("x", "z"), (Sum 1 empty, Arc))]))
invoke2Conditions = ConditionConj [(Condition (Lt "z1" "x1"))]

invoke3Terms = [(Sum 0 (fromList [("a", 1)])), (Sum (-1) (fromList [("a", -1), ("x", 1)]))]
invoke3Graph = Graph ["x", "a", "x1", "a1"] (fromList [(("x1", "a"), (Sum 0 empty, WeightedArc)), (("a", "x1"), (Sum 0 empty, WeightedArc)), (("x", "a1"), (Sum 1 (fromList [("a", 1)]), WeightedArc)), (("a1", "x"), (Sum (-1) (fromList [("a", (-1))]), WeightedArc)), (("x", "x1"), (Sum 1 empty, Arc)), (("x", "a"), (Sum 1 empty, Arc)), (("a1", "x1"), (Sum 1 empty, Arc)), (("a", "a1"), (Sum 1 (fromList [("x", (-1)), ("a", 2)]), WeightedArc)), (("a1", "a"), (Sum (-1) (fromList [("x", (1)), ("a", (-2))]), WeightedArc))])
invoke3Conditions = ConditionConj [(Condition (Lt "x1" "a1"))]

unit_handleOneInvokeConjunct = do
    (cleanGraph (handleOneInvokeConjunct invoke1Terms ["x1", "y1", "z1"] ["x", "y", "z"] invoke1Conditions) ["x", "y", "z"]) @?= invoke1Graph
    (cleanGraph (handleOneInvokeConjunct invoke2Terms ["x1", "y1", "z1"] ["x", "y", "z"] invoke2Conditions) ["x", "y", "z"]) @?= invoke2Graph
    (handleOneInvokeConjunct invoke3Terms ["x1", "a1"] ["x", "a"] invoke3Conditions) @?= invoke3Graph


oneConjunct1Graph = Graph ["x1", "x2", "x3"] (fromList [(("x1", "x3"), (Sum 3 empty, WeightedArc)), (("x3", "x1"), (Sum (-3) empty, WeightedArc))])
oneConjunct1MapVars = fromList [("x", "x1"), ("a", "x2"), ("y", "x3")] 


-- data AnnotatedDef g a = AnnotatedDef
--   { getName :: String
--   , getArgs :: [a]
--   , getBody :: g a
--   , getAnnotations :: [AnnotationType]
--   }

oneConjunct2MapDefs = fromList [("fun", AnnotatedDef "fun" ["x", "y", "z"] ((Sum 0 empty) :=: (Sum 1 empty)) [Dynamic, Dynamic, Dynamic])] 
oneConjunct2MapConditions = fromList [("fun", ConditionDisj [ConditionConj [(Condition (Eq "y" "z")), (Condition (Eq "z" "y")), (Condition (Lt "y" "x"))]])]

oneConjunct3MapConditions = fromList [("fun", ConditionDisj [ConditionConj [(Condition (Lt "z" "x"))]])]

oneConjunct4MapDefs = fromList [("fun", AnnotatedDef "fun" ["x", "a"] ((Sum 0 empty) :=: (Sum 1 empty)) [Dynamic, Dynamic, Dynamic])] 
oneConjunct4MapConditions = fromList [("fun", ConditionDisj [ConditionConj [(Condition (Lt "x" "a"))]])]
oneConjunct4Graph = Graph ["x", "a", "0", "1"] (fromList [(("0", "a"), (Sum 0 empty, WeightedArc)), (("a", "0"), (Sum 0 empty, WeightedArc)), (("x", "1"), (Sum 1 (fromList [("a", 1)]), WeightedArc)), (("1", "x"), (Sum (-1) (fromList [("a", (-1))]), WeightedArc)), (("x", "0"), (Sum 1 empty, Arc)), (("x", "a"), (Sum 1 empty, Arc)), (("1", "0"), (Sum 1 empty, Arc)), (("a", "1"), (Sum 1 (fromList [("x", (-1)), ("a", 2)]), WeightedArc)), (("1", "a"), (Sum (-1) (fromList [("x", (1)), ("a", (-2))]), WeightedArc))])

unit_goOneConjunct = do 
    (fst $ runState (goOneConjunct ((Sum 1 (fromList [("x", 1), ("a", 1)])) :=: (Sum 4 (fromList [("a", 1), ("y", 1)]))) empty empty ["x1", "x2", "x3"] oneConjunct1MapVars) defaultNames) @?= [oneConjunct1Graph]
    (cleanGraph ((fst $ runState (goOneConjunct (Invoke "fun" invoke1Terms Ann.Unfold) oneConjunct2MapDefs oneConjunct2MapConditions ["x", "y", "z"] (fromList [("x", "x"), ("y", "y"), ("z", "z")])) defaultNames)!! 0) ["x", "y", "z"]) @?= invoke1Graph
    (cleanGraph ((fst $ runState (goOneConjunct (Invoke "fun" invoke2Terms Ann.Unfold) oneConjunct2MapDefs oneConjunct3MapConditions ["x", "y", "z"] (fromList [("x", "x"), ("y", "y"), ("z", "z")])) defaultNames)!! 0) ["x", "y", "z"]) @?= invoke2Graph
    ((fst $ runState (goOneConjunct (Invoke "fun" invoke3Terms Ann.Unfold) oneConjunct4MapDefs oneConjunct4MapConditions ["x", "a"] (fromList [("x", "x"), ("a", "a")])) defaultNames) !! 0) @?= oneConjunct4Graph

goBody1MapConditions = fromList [("fun", ConditionDisj [ConditionConj [(Condition (Eq "y" "z")), (Condition (Eq "z" "y")), (Condition (Lt "y" "x"))]]), ("fun1", ConditionDisj [ConditionConj [(Condition (Lt "z" "x"))]])]
goBody1MapDefs = fromList [("fun", AnnotatedDef "fun" ["x", "y", "z"] ((Sum 0 empty) :=: (Sum 1 empty)) [Dynamic, Dynamic, Dynamic]), ("fun1", AnnotatedDef "fun1" ["x", "y", "z"] ((Sum 0 empty) :=: (Sum 1 empty)) [Dynamic, Dynamic, Dynamic])] 
goBody1MapVars = fromList [("x", "x"), ("y", "y"), ("z", "z")]
goBody1Graph = (Graph ["x", "y", "z"] (fromList [(("x", "z"), (Sum 1 empty, Arc))]))
goBody1MapConditionsRes = ConditionDisj [ConditionConj [(Condition (Lt "z" "x"))]]
tr = (goBody (Conjunction (Invoke "fun" invoke1Terms Ann.Unfold) (Invoke "fun1" invoke2Terms Ann.Unfold) []) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars)
tr1 = goBody (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun1" invokeBodyTerms Ann.Unfold) []) goBody1MapDefs goBody1MapConditions ["x", "y", "z", "x'"] ["x", "y", "z", "x'"] defaultNames goBody2MapVars
tr2 = goBody (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun1" invokeBody2Terms Ann.Unfold) [(Sum 0 (fromList [("y'", 1)])) :=: (Sum 1 (fromList [("z", 2)]))]) goBody1MapDefs goBody1MapConditions ["x", "y", "z", "x'", "y'"] ["x", "y", "z", "x'", "y'"] defaultNames goBody3MapVars
tr3 = (goBody (Fresh "x'" (Fresh "y'" (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun" invokeBody2Terms Ann.Unfold) [(Sum 0 (fromList [("y'", 1)])) :=: (Sum 1 (fromList [("z", 2)]))]))) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars) 
tr4 = goBody (Invoke "fun" invoke1Terms Ann.Unfold) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars

invokeBodyTerms = [(Sum 0 (fromList [("x'", 1)])), (Sum 0 (fromList [("x", 1)])), (Sum 1 (fromList [("z", 2)]))]
goBody2MapVars = fromList [("x", "x"), ("y", "y"), ("z", "z"), ("x'", "x'")]
goBody2MapConditionsRes = ConditionDisj [ConditionConj [(Condition (Eq "x" "x")), (Condition (Eq "y" "y")), (Condition (Eq "x'" "x'")), (Condition (Lt "x" "x'")), (Condition (Lt "z" "x'")), (Condition (Lt "y" "x'"))]] 

invokeBody2Terms = [(Sum 0 (fromList [("x'", 1)])), (Sum 0 (fromList [("x", 1)])), (Sum 0 (fromList [("y'", 1)]))]
goBody3MapVars = fromList [("x", "x"), ("y", "y"), ("z", "z"), ("x'", "x'"), ("y'", "y'")]
goBody3MapConditionsRes = ConditionDisj [ConditionConj [(Condition (Eq "x" "x")), (Condition (Eq "z" "z")), (Condition (Eq "y'" "y'")), (Condition (Lt "z" "y'")), (Condition (Lt "y'" "x'")), (Condition (Eq "y" "y")), (Condition (Eq "x'" "x'")), (Condition (Lt "x" "x'")), (Condition (Lt "z" "x'")), (Condition (Lt "y" "x'"))]]



unit_goBody = do 
    (equalConds (trace (show tr) $ goBody (Conjunction (Invoke "fun" invoke1Terms Ann.Unfold) (Invoke "fun1" invoke2Terms Ann.Unfold) []) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars) goBody1MapConditionsRes) @?= True
    (equalConds (trace (show tr1) $ goBody (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun1" invokeBodyTerms Ann.Unfold) []) goBody1MapDefs goBody1MapConditions ["x", "y", "z", "x'"] ["x", "y", "z", "x'"] defaultNames goBody2MapVars) goBody2MapConditionsRes) @?= True
    (equalConds (trace (show tr2) $ goBody (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun1" invokeBody2Terms Ann.Unfold) [(Sum 0 (fromList [("y'", 1)])) :=: (Sum 1 (fromList [("z", 2)]))]) goBody1MapDefs goBody1MapConditions ["x", "y", "z", "x'", "y'"] ["x", "y", "z", "x'", "y'"] defaultNames goBody3MapVars) goBody3MapConditionsRes) @?= True
    (equalConds (trace (show tr3) $ goBody (Fresh "x'" (Fresh "y'" (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun" invokeBody2Terms Ann.Unfold) [(Sum 0 (fromList [("y'", 1)])) :=: (Sum 1 (fromList [("z", 2)]))]))) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars) goBody1MapConditionsRes) @?= True
    (equalConds (goBody (Fresh "x'" (Fresh "y'" (Conjunction ((Sum 0 (fromList [("x'", 1)])) :=: (Sum 1 (fromList [("x", 1), ("y", 1)]))) (Invoke "fun" invokeBody2Terms Ann.Unfold) [(Sum 0 (fromList [("y'", 1)])) :=: (Sum 1 (fromList [("z", 2)]))]))) goBody1MapDefs empty ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars) (ConditionDisj [])) @?= True
    (equalConds (trace (show tr4) $ goBody (Invoke "fun" invoke1Terms Ann.Unfold) goBody1MapDefs goBody1MapConditions ["x", "y", "z"] ["x", "y", "z"] defaultNames goBody1MapVars) goBody1MapConditionsRes) @?= True

defAppend = AnnotatedDef "append" ["x", "y", "z"] (Disjunction (Conjunction ((Sum 0 (fromList [("x", 1)])) :=: (Sum 0 empty)) ((Sum 0 (fromList [("y", 1)])) :=: (Sum 0 (fromList [("z", 1)]))) []) (Invoke "append" [(Sum (-1) (fromList [("x", 1)])), (Sum (0) (fromList [("y", 1)])), (Sum (-1) (fromList [("z", 1)]))] Ann.Unfold) []) [Dynamic, Dynamic, Static]
condsResAppend = ConditionDisj [ConditionConj [(Condition (Eq "y" "z"))]]
resMapAppend1 = fromList [("append", ConditionDisj [ConditionConj [Condition (Eq "y" "y"),Condition (Eq "y" "z"),Condition (Eq "z" "y"),Condition (Eq "z" "z")]])]

tr5 = (goOneDef defAppend (fromList [("append", defAppend)]) empty) 

unit_goOneCycle = do 
    trace ("AAAAA" ++ show tr5) $ show (goOneDef defAppend (fromList [("append", defAppend)]) empty) @?= show resMapAppend1
    trace ("BBBBB" ++ show tr5) $ show (goOneCycle [defAppend] (fromList [("append", defAppend)]) empty) @?= show resMapAppend1