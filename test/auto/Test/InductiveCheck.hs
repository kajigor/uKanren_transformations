{-# LANGUAGE FlexibleContexts #-}

module Test.InductiveCheck where

import Syntax
import Program
import InductiveCheck
import Program.List
import Program.Num
import Test.Helper ((@?=), test)

unit_orderTest = do
  False @?= (x <<= y)
  True  @?= (x <<= x)
  True  @?= (x <<= xyz)
  False @?= (xyz <<= x)
  False @?= (xy <<= xyz)
  True  @?= (yz <<= xyz)
  True  @?= (emptyTree <<= tree)
  True  @?= (subtree <<= tree)
  True  @?= (tree <<= tree)
  False @?= (tree <<= subtree)

unit_isInductiveTest = do
    listFunctionsTest
    peanoFunctionsTest
  where
    listFunctionsTest = do
      test isInductive app True
      test isInductive rev True
      test isInductive mem True
      test isInductive cop True
      test isInductive cop2 True
      test isInductive len True
      test isInductive assoc True

    app   = Program appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])
    rev   = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
    mem   = Program membero $ fresh ["x", "y"] (call "membero" [V "x", V "y"])
    cop   = Program copy $ fresh ["x", "y"] (call "copy" [V "x", V "y"])
    cop2  = Program copycopy $ fresh ["x", "y"] (call "copy2" [V "x", V "y"])
    len   = Program lengtho $ fresh ["x", "y"] (call "lengtho" [V "x", V "y"])
    assoc = Program assoco $ fresh ["x", "y", "z"] (call "assoco" [V "x", V "y", V "z"])

    peanoFunctionsTest = do
      test isInductive le True
      test isInductive gt True

    le = Program leo $ fresh ["x", "y", "z"] (call "leo" [V "x", V "y", V "z"])
    gt = Program gto $ fresh ["x", "y", "z"] (call "gto" [V "x", V "y", V "z"])


[x, y, z] = map V ["x", "y", "z"]

cons h t = C "cons" [h, t]
fromList xs = foldr cons nil xs
xy  = fromList [x, y]
yz  = fromList [y, z]
xyz = fromList [x, y, z]

node = C "node"
tree = node [node [], node [node [x], node [y, z]], node [x, y]]
emptyTree = node []
subtree = node [y, z]

t <<= u = IndWrap t <= IndWrap u
