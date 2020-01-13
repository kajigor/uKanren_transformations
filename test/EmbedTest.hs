module EmbedTest where

import Syntax
import Driving
import Util.Miscellaneous

checkStep = Invoke "cS"
step = Invoke "s"
getAnswer = Invoke "gA"
getTime = Invoke "gT"
add = Invoke "a"

checkPerson = Invoke "cP"
movePerson = Invoke "mP"
moveLight = Invoke "mL"
times = Invoke "t"

ololo  = Invoke "ololo"

g1 = [checkStep([C "St" [C "T" [], C "T" [], C "T" []],V 3,C "T" []]) , step([C "St" [C "T" [], C "T" [], C "T" []],V 3,V 7]) ,  getAnswer([V 4, V 7, C "some" [V 8]])  , getTime([V 3, V 10]) , add([V 10,V 8, V 9])]
g2 = [checkPerson([C "St" [C "T" [], C "T" [], C "T" []],V 36,C "T" []]) , movePerson([C "St" [C "T" [], C "T" [], C "T" []],V 36,V 21]) , moveLight([V 21,V 7]) , checkStep([V 7,                                 V 26,C "T" []]) , step([V 7,                                 V 26,V 30]) , getAnswer([V 27,V 30,C "some" [V 31]]) , getTime([V 26,V 33]) , add([V 33,V 31,V 32]) , times([V 36,C "S" [V 41]]) , add([V 41,C "S" [V 32],V 9])]
g3 = [checkPerson([C "St" [C "T" [], C "T" [], C "T" []],V 36,C "T" []]) , movePerson([C "St" [C "T" [], C "T" [], C "T" []],V 36,V 21]) , moveLight([V 21,V 7]) , checkStep([V 7,                                 V 26,C "T" []]) , ololo([]), step([V 7,                                 V 26,V 30]) , getAnswer([V 27,V 30,C "some" [V 31]]) , getTime([V 26,V 33]) , add([V 33,V 31,V 32]) , times([V 36,C "S" [V 41]]) , add([V 41,C "S" [V 32],V 9])]

test = embedGoals g1 g2

test' = map (map trd3) (split (map (\ x -> (undefined, undefined, x)) g3) g1)
