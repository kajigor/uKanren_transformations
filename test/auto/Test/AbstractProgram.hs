
module Test.AbstractProgram where

import           Test.Helper 
import qualified BTA.SizeConversion as ABS
import qualified InvokeAnnotation as InvA
import           AnnotatedProgram
import           AnnotatedDef
import qualified Syntax as S
import           AnnotationType
import           Term
import           Util.Miscellaneous     (mapLeft)
import qualified Parser.Parser as Parser
import qualified Parser.AnnotatedParser as AnnotatedParser
import           Data.Either
import qualified Data.Map as Map

getAnnotationParser :: (String -> IO (Either String (AnnotatedProgram S.G S.X)))
getAnnotationParser input = do 
  res <- Parser.parseImports AnnotatedParser.parseProgramWithImports input
  return $ mapLeft show res

sumAnn :: AnnotatedProgram S.G S.X
sumAnn = AnnotatedProgram ([
    (AnnotatedDef "fail" [] (S.Invoke "fail" []) []),
    (AnnotatedDef "addo" ["x", "y", "z"] (
    S.Disjunction (S.Conjunction ((S.V "x") S.:=: (S.C "Zero" [])) ((S.V "z") S.:=: (S.V "y")) [])
     (S.Fresh "x'" (S.Conjunction ((S.V "x") S.:=: (S.C "Succ" [S.V "x'"])) (S.Invoke "addo" [(S.V "x'"), (S.C "Succ" [S.V "y"]), (S.V "z")]) []))
     []
     )
      [Static, Dynamic, Dynamic]), 
      (AnnotatedDef "evalo" ["fm", "r"] (
        S.Disjunction ((S.V "fm") S.:=: (S.C "Num" [(S.V "r")])) 
        (S.Fresh "x" (S.Fresh "y" (S.Fresh "xr" (S.Fresh "yr" (
            S.Conjunction (S.Invoke "evalo" [(S.V "x"), (S.V "xr")]) 
                (S.Invoke "evalo" [(S.V "y"), (S.V "yr")])
                [((S.V "fm") S.:=: (S.C "Sum" [(S.V "x"), (S.V "y")])), (S.Invoke "addo" [(S.V "xr"), (S.V "yr"), (S.V "r")])]
            )
        )))) []
      ) [Static, Dynamic])
      ]) (S.Fresh "y" (S.Invoke "addo" [(S.C "Zero" []), (S.V "y"), (S.C "Succ" [(S.C "Succ" [(S.C "Zero" [])])])]))

sumAnnInvoke :: AnnotatedProgram InvA.AnnG S.X
sumAnnInvoke = AnnotatedProgram ([
    (AnnotatedDef "fail" [] (InvA.Invoke "fail" [] InvA.Memo) []),
    (AnnotatedDef "addo" ["x", "y", "z"] (
    InvA.Disjunction (InvA.Conjunction ((S.V "x") InvA.:=: (S.C "Zero" [])) ((S.V "z") InvA.:=: (S.V "y")) [])
     (InvA.Fresh "x'" (InvA.Conjunction ((S.V "x") InvA.:=: (S.C "Succ" [S.V "x'"])) (InvA.Invoke "addo" [(S.V "x'"), (S.C "Succ" [S.V "y"]), (S.V "z")] InvA.Memo) []))
     []
     )
      [Static, Dynamic, Dynamic]), 
      (AnnotatedDef "evalo" ["fm", "r"] (
        InvA.Disjunction ((S.V "fm") InvA.:=: (S.C "Num" [(S.V "r")])) 
        (InvA.Fresh "x" (InvA.Fresh "y" (InvA.Fresh "xr" (InvA.Fresh "yr" (
            InvA.Conjunction (InvA.Invoke "evalo" [(S.V "x"), (S.V "xr")] InvA.Memo) 
                (InvA.Invoke "evalo" [(S.V "y"), (S.V "yr")] InvA.Memo)
                [((S.V "fm") InvA.:=: (S.C "Sum" [(S.V "x"), (S.V "y")])), (InvA.Invoke "addo" [(S.V "xr"), (S.V "yr"), (S.V "r")] InvA.Memo)]
            )
        )))) []
      ) [Static, Dynamic])
      ]) (InvA.Fresh "y" (InvA.Invoke "addo" [(S.C "Zero" []), (S.V "y"), (S.C "Succ" [(S.C "Succ" [(S.C "Zero" [])])])] InvA.Memo))

unit_parseAnnotations = do 
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/sum.mk"
    program @?= (Right sumAnn :: Either String (AnnotatedProgram S.G S.X) )
    program1 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/prop.mk"
    let annotations = either (\x -> [[]]) (\x -> (map getAnnotations) $ getDefs x) program1
    annotations @?= [[], [Static, Static, Dynamic], [Static, Static, Dynamic], [Static, Static], [Static, Static, Dynamic], [Static, Static, Dynamic], [Static, Static, Dynamic]] 
    program2 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/incorrect.mk"
    isLeft program2 @?= True
    program3 <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/num.mk"
    let annotations3 = either (\x -> [[]]) (\x -> (map getAnnotations) $ getDefs x) program3
    annotations3 @?= [[], [Dynamic], [Static, Dynamic, Dynamic], [Static, Static, Dynamic], [Static, Static, Dynamic], [Dynamic, Static, Static], [Static, Static, Dynamic], [Static, Dynamic, Static]] 

oneAnnInvoke = (InvA.Fresh "h" (InvA.Fresh "t" (InvA.Fresh "n'"
    (InvA.Disjunction 
        (InvA.Conjunction 
            ((S.V "n") InvA.:=: (S.C "Zero" [])) 
            ((S.V "s") InvA.:=: (S.C "Cons" [(S.V "h"), (S.V "t")])) 
            [(S.V "v") InvA.:=: (S.V "h")]) 
        (InvA.Conjunction 
            ((S.V "s") InvA.:=: (S.C "Cons" [(S.V "h"), (S.V "t")]))
            (InvA.Delay (InvA.Invoke "elemo" [(S.V "n'"), (S.V "t"), (S.V "v")] InvA.Memo)) 
            [(S.V "n") InvA.:=: (S.C "Succ" [(S.V "n'")])]) 
        []) 
    )))

unit_annotateInvokes = do
    let sumAnnInvoke1 = InvA.annotateInvokesPr sumAnn
    sumAnnInvoke1 @?= sumAnnInvoke 
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/prop.mk"
    let propAnnInvokeDefs = either (\x -> []) (\x -> (map getBody) $ getDefs $ InvA.annotateInvokesPr x) program
    (propAnnInvokeDefs !! 6) @?= oneAnnInvoke

sumAnnAbstract :: AnnotatedProgram ABS.AbstractG S.X
sumAnnAbstract = AnnotatedProgram ([
    (AnnotatedDef "fail" [] (ABS.Invoke "fail" [] InvA.Unfold) []),
    (AnnotatedDef "addo" ["x", "y", "z"] (
    ABS.Disjunction (ABS.Conjunction ((ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 Map.empty)) ((ABS.Sum 0 (Map.fromList [("z", 1)])) ABS.:=: (ABS.Sum 0 (Map.fromList [("y", 1)]))) [])
     (ABS.Fresh "x'" (ABS.Conjunction ((ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("x'", 1)]))) (ABS.Invoke "addo" [(ABS.Sum 0 (Map.fromList [("x'", 1)])), (ABS.Sum 1 (Map.fromList [("y", 1)])), (ABS.Sum 0 (Map.fromList [("z", 1)]))] InvA.Unfold) []))
     []
     )
      [Static, Dynamic, Dynamic]), 
      (AnnotatedDef "evalo" ["fm", "r"] (
        ABS.Disjunction ((ABS.Sum 0 (Map.fromList [("fm", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("r", 1)]))) 
        (ABS.Fresh "x" (ABS.Fresh "y" (ABS.Fresh "xr" (ABS.Fresh "yr" (
            ABS.Conjunction (ABS.Invoke "evalo" [(ABS.Sum 0 (Map.fromList [("x", 1)])), (ABS.Sum 0 (Map.fromList [("xr", 1)]))] InvA.Unfold) 
                (ABS.Invoke "evalo" [(ABS.Sum 0 (Map.fromList [("y", 1)])), (ABS.Sum 0 (Map.fromList [("yr", 1)]))] InvA.Unfold)
                [((ABS.Sum 0 (Map.fromList [("fm", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("x", 1), ("y", 1)]))), (ABS.Invoke "addo" [(ABS.Sum 0 (Map.fromList [("xr", 1)])), (ABS.Sum 0 (Map.fromList [("yr", 1)])), (ABS.Sum 0 (Map.fromList [("r", 1)]))] InvA.Unfold)]
            )
        )))) []
      ) [Static, Dynamic])
      ]) (ABS.Fresh "y" (ABS.Invoke "addo" [(ABS.Sum 1 Map.empty), (ABS.Sum 0 (Map.fromList [("y", 1)])), (ABS.Sum 3 Map.empty)] InvA.Unfold))

appendoAbst :: ABS.AbstractG S.X
appendoAbst = (
    ABS.Disjunction (ABS.Conjunction ((ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 Map.empty)) ((ABS.Sum 0 (Map.fromList [("y", 1)])) ABS.:=: (ABS.Sum 0 (Map.fromList [("xy", 1)]))) []) 
        ( ABS.Fresh "h" (ABS.Fresh "t" (ABS.Fresh "ty" (
            ABS.Conjunction (
                (ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("h", 1), ("t", 1)]))
            ) (
                (ABS.Sum 0 (Map.fromList [("xy", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("h", 1), ("ty", 1)]))
            ) [
                ABS.Invoke "appendo" [(ABS.Sum 0 (Map.fromList [("t", 1)])), (ABS.Sum 0 (Map.fromList [("y", 1)])), (ABS.Sum 0 (Map.fromList [("ty", 1)]))] InvA.Memo
            ]
       ))) 
    )
    [] ) 

reversoAbst :: ABS.AbstractG S.X
reversoAbst = (
    ABS.Disjunction (ABS.Conjunction ((ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 Map.empty)) ((ABS.Sum 0 (Map.fromList [("y", 1)])) ABS.:=: (ABS.Sum 1 Map.empty)) [])
       (ABS.Fresh "h" (ABS.Fresh "t" (ABS.Fresh "rt" (
            ABS.Conjunction (
                (ABS.Sum 0 (Map.fromList [("x", 1)])) ABS.:=: (ABS.Sum 1 (Map.fromList [("h", 1), ("t", 1)]))
            ) (
                (ABS.Invoke "reverso" [(ABS.Sum 0 (Map.fromList [("t", 1)])), (ABS.Sum 0 (Map.fromList [("rt", 1)]))] InvA.Memo)
            ) [
                (ABS.Invoke "appendo" [(ABS.Sum 0 (Map.fromList [("rt", 1)])), (ABS.Sum 2 (Map.fromList [("h", 1)])), (ABS.Sum 0 (Map.fromList [("y", 1)]))] InvA.Memo)
            ]
        )))
    )
    [] )


unit_convert = do
    let sumAnnAbstract1 = ABS.convert sumAnnInvoke
    sumAnnAbstract @?= sumAnnAbstract
    program <- getAnnotationParser "test/resources/newSyntax/withTypeAnnotations/list.mk"
    let propAnnAbstractDefs = either (\x -> []) (\x -> (map getBody) $ getDefs $ ABS.convert $ InvA.annotateInvokesPr x) program
    (propAnnAbstractDefs !! 1) @?= appendoAbst
    (propAnnAbstractDefs !! 2) @?= reversoAbst

unit_terms_eq = do
    test2 (==) (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 1)])) True
    test2 (==) (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 1), ("b", 2)])) False
    test2 (==) (ABS.Sum 1 (Map.fromList [("a", 1), ("c", 2)])) (ABS.Sum 1 (Map.fromList [("a", 1), ("b", 2)])) False

unit_terms_weight = do
    (ABS.getWeight (ABS.Sum 1 (Map.fromList [("a", 1), ("b", 2)])) (ABS.Sum 1 (Map.fromList [("a", 1)]))) @?= (ABS.Sum 0 (Map.fromList [("b", 2)]))
    (ABS.getWeight (ABS.Sum 1 (Map.fromList [("a", 2)])) (ABS.Sum 1 (Map.fromList [("a", 1)]))) @?= (ABS.Sum 0 (Map.fromList [("a", 1)]))
    (ABS.getWeight (ABS.Sum 1 (Map.fromList [("a", 2)])) (ABS.Sum 2 (Map.fromList [("a", 1)]))) @?= (ABS.Sum (-1) (Map.fromList [("a", 1)]))
    (ABS.getWeight (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 1)]))) @?= (ABS.Sum 0 (Map.empty :: (Map.Map String Int)))

unit_terms_less = do
    (ABS.isLess (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 1), ("b", 2)]))) @?= True
    (ABS.isLess (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 2)]))) @?= True
    (ABS.isLess (ABS.Sum 2 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 2)]))) @?= False
    (ABS.isLess (ABS.Sum 1 (Map.fromList [("a", 1)])) (ABS.Sum 1 (Map.fromList [("a", 1)]))) @?= False