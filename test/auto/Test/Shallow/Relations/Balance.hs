{-# LANGUAGE ApplicativeDo #-}
module Test.Shallow.Relations.Balance(deptho, balancedo, balanceo) where


import Shallow.Core.Kanren
import Test.Shallow.Types.Nat
import Test.Shallow.Types.Tree
import Test.Shallow.Types.List
import Test.Shallow.Relations.Arith

deptho :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L Natural rel -> Relation rel
deptho = relation2 "deptho" $ \t d -> conde 
    [ do
        t <=> leaf
        d <=> zro
        pure ()
    , fresh3 $ \l x r -> fresh3 $ \ld rd d' -> do
        t <=> node l x r
        d <=> suc d'
        call $ deptho l ld
        call $ deptho r rd
        call $ maxo ld rd d'
        pure ()
    ]

balancedo :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> Relation rel
balancedo = relation "balancedo" $ \t -> conde 
    [ t <=> leaf
    , fresh5 $ \l x r dl dr -> do
        t <=> node l x r
        call $ deptho l dl
        call $ deptho r dr
        call $ similaro dl dr
        call $ balancedo l
        call $ balancedo r
        pure ()
    ]

appendo :: (Kanren rel, LogicType elem) => L [elem] rel -> L [elem] rel -> L [elem] rel -> Relation rel
appendo = relation3 "appendo" $ \x y xy -> conde
    [ do
        x <=> nil
        y <=> xy
        pure ()
    , fresh3 $ \h x' xy' -> do
        x <=> cons h x'
        xy <=> cons h xy'
        call $ appendo x' y xy'
        pure ()
    ]

traverso :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L [elem] rel -> Relation rel
traverso = relation2 "traverso" $ \t e -> conde 
    [ do
        t <=> leaf
        e <=> nil
        pure ()
    , fresh5 $ \l x r el er -> do
        t <=> node l x r
        call $ traverso l el
        call $ traverso r er
        call $ appendo el (cons x er) e
        pure ()
    ]

balanceo :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L (Tree elem) rel -> Relation rel
balanceo = relation2 "balanceo" $ \v u -> fresh $ \e -> do
    call $ traverso v e
    call $ traverso u e
    call $ balancedo u
    pure ()