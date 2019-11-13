{-# LANGUAGE FlexibleContexts #-}

module InductiveCheckTest where 

import Syntax 
import InductiveCheck 
import Text.Printf
import TestFramework

import List 
import Num

termOrderTest = do 
  putStrLn "termOrderTest\n"
  assert "1"  False (x <<= y)
  assert "2"  True  (x <<= x)
  assert "3"  True  (x <<= xyz)
  assert "4"  False (xyz <<= x)
  assert "5"  False (xy <<= xyz)
  assert "6"  True  (yz <<= xyz)

  assert "7"  True  (emptyTree <<= tree)
  assert "8"  True  (subtree <<= tree)
  assert "9"  True  (tree <<= tree)
  assert "10" False (tree <<= subtree)

isInductiveTest = do 
  putStrLn "isInductiveTest\n"
  listFunctionsTest
  peanoFunctionsTest
  
  where 
    listFunctionsTest = do 
      putStrLn "List functions"
      assert "appendo" True (isInductive app)
      assert "revAcco" True (isInductive rev)
      assert "membero" True (isInductive mem)
      assert "copy"    True (isInductive cop)
      assert "copy2"   True (isInductive cop2)
      assert "lengtho" True (isInductive len)
      assert "assoco"  True (isInductive assoc)

    app = appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])
    rev = revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
    mem = membero $ fresh ["x", "y"] (call "membero" [V "x", V "y"])
    cop = copy $ fresh ["x", "y"] (call "copy" [V "x", V "y"])
    cop2 = copy2 $ fresh ["x", "y"] (call "copy2" [V "x", V "y"])
    len = lengtho $ fresh ["x", "y"] (call "lengtho" [V "x", V "y"])
    assoc = assoco $ fresh ["x", "y", "z"] (call "assoco" [V "x", V "y", V "z"])

    peanoFunctionsTest = do 
      putStrLn "Peano functions"
      assert "leo" True (isInductive le)
      assert "gto" True (isInductive gt)

    le = leo $ fresh ["x", "y", "z"] (call "leo" [V "x", V "y", V "z"])
    gt = gto $ fresh ["x", "y", "z"] (call "gto" [V "x", V "y", V "z"])
    

main = do
  termOrderTest
  isInductiveTest



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

strip (Let def goal) = def 
strip _ = error "Failed to retrieve a definition"