module Hypergraph.Converter where

import qualified Hypergraph.Hypergraph as H

import qualified Purification as P
import qualified Util.ToProlog as P

import Def
import qualified Syntax as S

funcToAtom :: P.Func -> H.Atom String
funcToAtom (name, args) = H.Atom name args

prologToRule :: P.Rule -> H.Rule String
prologToRule (head, body) = (funcToAtom head, map funcToAtom body)

rulesToProgram :: P.Rules -> H.Program String
rulesToProgram = map prologToRule

defsToProgram :: [Def S.G String] -> H.Program String 
defsToProgram = rulesToProgram . concatMap P.defToProlog
