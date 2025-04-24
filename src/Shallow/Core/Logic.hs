
module Shallow.Core.Logic
(
  Var
, Logic(Free, Ground)
, NoVars
, LogicType(unifyVal, derefVal, reify, project)
, vmapM, vmapMVal
, reify', project'
, showLogic
) where

import Shallow.Core.Internal.Logic
import Shallow.Utils.Logic