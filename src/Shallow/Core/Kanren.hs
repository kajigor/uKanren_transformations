module Shallow.Core.Kanren
(
  Relation(Relation)
, Kanren(KVar)
, KanrenEval
, LogicType
, L
, fresh, fresh2, fresh3, fresh4, fresh5
, relation, relation2, relation3, relation4, relation5
, call, embed
, eval
, run, run2, run3, run4, run5
, (===), (<=>)
, conde
) where

import Shallow.Core.Internal.Kanren
import Shallow.Utils.Kanren
import Shallow.Core.Internal.Logic