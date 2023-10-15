containso y0 = fresh q1 q2 q3 q4 (((y0 = (1 :: (q1 :: q2)) /\ newoCono q1 q2) \/ (y0 = (q3 :: q4) /\ _cono q4)))
newoCono y3 y4 = fresh q1 q2 (((y4 = (q1 :: q2) /\ y3 = 0 /\ _newoCono q1 q2) \/ __appendoAppendoAppendoCono y3 y4))
_newoCono y7 y8 = ((y7 = 2 /\ cono y8) \/ appendoAppendoAppendoCono y7 y8)
cono y9 = success
appendoAppendoAppendoCono y12 y13 = (_cono y13 \/ _appendoAppendoAppendoCono y12 y13)
_appendoAppendoAppendoCono y17 y18 = fresh q1 q2 ((y18 = (q1 :: q2) /\ newoCono q1 q2 /\ appendo y17))
appendo y22 = y22 = 1
__appendoAppendoAppendoCono y26 y27 = fresh q1 q2 ((_cono y27 \/ (y27 = (q1 :: q2) /\ newoCono q1 q2 /\ appendo y26)))
_cono y30 = fresh q1 q2 q3 q4 (((y30 = (1 :: (q1 :: q2)) /\ newoCono q1 q2) \/ (y30 = (q3 :: q4) /\ _cono q4)))

containso x1