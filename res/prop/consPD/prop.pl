oroEvaloEvalo(lit(trueo), lit(trueo)).
oroEvaloEvalo(lit(trueo), disj(Q1, Q2)) :- oroEvaloEvalo(Q1, Q2).
oroEvaloEvalo(lit(trueo), conj(lit(trueo), lit(trueo))).
oroEvaloEvalo(lit(trueo), conj(lit(trueo), disj(Q3, Q4))) :- oroEvaloEvalo(Q3, Q4).
oroEvaloEvalo(lit(trueo), conj(lit(trueo), conj(Q3, Q4))) :- andoEvaloEvalo(Q3, Q4).
oroEvaloEvalo(lit(trueo), conj(disj(Q5, Q6), lit(trueo))) :- oroEvaloEvalo(Q5, Q6).
oroEvaloEvalo(lit(trueo), conj(disj(Q5, Q6), disj(Q7, Q8))) :- oro(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10), oro(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12).
oroEvaloEvalo(lit(trueo), conj(disj(Q5, Q6), conj(Q7, Q8))) :- oro(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10), _ando(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12).
oroEvaloEvalo(lit(trueo), conj(conj(Q5, Q6), lit(trueo))) :- andoEvaloEvalo(Q5, Q6).
oroEvaloEvalo(lit(trueo), conj(conj(Q5, Q6), disj(Q13, Q14))) :- _ando(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10), oro(Q15, Q16), _evalo(Q13, Q15), _evalo(Q14, Q16).
oroEvaloEvalo(lit(trueo), conj(conj(Q5, Q6), conj(Q13, Q14))) :- _ando(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10), _ando(Q15, Q16), _evalo(Q13, Q15), _evalo(Q14, Q16).
oroEvaloEvalo(var(Q19), disj(Q17, Q18)).
oroEvaloEvalo(var(Q19), conj(Q17, Q18)).
oroEvaloEvalo(disj(Q20, Q21), lit(trueo)) :- oroEvaloEvalo(Q20, Q21).
oroEvaloEvalo(disj(Q20, Q21), var(Q22)).
oroEvaloEvalo(disj(Q20, Q21), disj(Q23, Q24)) :- oroEvaloEvaloOroEvaloEvalo(Q20, Q21, Q23, Q24).
oroEvaloEvalo(disj(Q20, Q21), conj(Q23, Q24)) :- oroEvaloEvaloAndoEvaloEvalo(Q20, Q21, Q23, Q24).
oroEvaloEvalo(conj(Q20, Q21), lit(trueo)) :- andoEvaloEvalo(Q20, Q21).
oroEvaloEvalo(conj(Q20, Q21), var(Q25)).
oroEvaloEvalo(conj(Q20, Q21), disj(Q26, Q27)) :- andoEvaloEvaloOroEvaloEvalo(Q20, Q21, Q26, Q27).
oroEvaloEvalo(conj(Q20, Q21), conj(Q26, Q27)) :- andoEvaloEvaloAndoEvaloEvalo(Q20, Q21, Q26, Q27).
oroEvaloEvalo(lit(falso), lit(trueo)).
oroEvaloEvalo(lit(falso), disj(Q28, Q29)) :- oroEvaloEvalo(Q28, Q29).
oroEvaloEvalo(lit(falso), conj(Q28, Q29)) :- andoEvaloEvalo(Q28, Q29).
oroEvaloEvalo(disj(lit(falso), lit(falso)), lit(trueo)).
oroEvaloEvalo(disj(lit(falso), disj(Q32, Q33)), lit(trueo)) :- _oroEvaloEvalo(Q32, Q33).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), lit(trueo))), lit(trueo)).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), disj(Q34, Q35))), lit(trueo)) :- oroEvaloEvalo(Q34, Q35).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), conj(Q34, Q35))), lit(trueo)) :- andoEvaloEvalo(Q34, Q35).
oroEvaloEvalo(disj(lit(falso), conj(var(Q38), disj(Q36, Q37))), lit(trueo)).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q39, Q40), lit(trueo))), lit(trueo)) :- _oroEvaloEvalo(Q39, Q40).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q39, Q40), disj(Q41, Q42))), lit(trueo)) :- __oro(Q43, Q44), _evalo(Q39, Q43), _evalo(Q40, Q44), oro(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q39, Q40), conj(Q41, Q42))), lit(trueo)) :- __oro(Q43, Q44), _evalo(Q39, Q43), _evalo(Q40, Q44), _ando(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q39, Q40), lit(trueo))), lit(trueo)) :- _andoEvaloEvalo(Q39, Q40).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q39, Q40), disj(Q47, Q48))), lit(trueo)) :- __ando(Q43, Q44), _evalo(Q39, Q43), _evalo(Q40, Q44), oro(Q49, Q50), _evalo(Q47, Q49), _evalo(Q48, Q50).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q39, Q40), conj(Q47, Q48))), lit(trueo)) :- __ando(Q43, Q44), _evalo(Q39, Q43), _evalo(Q40, Q44), _ando(Q49, Q50), _evalo(Q47, Q49), _evalo(Q48, Q50).
oroEvaloEvalo(disj(lit(falso), conj(lit(trueo), lit(falso))), lit(trueo)).
oroEvaloEvalo(disj(lit(falso), conj(lit(trueo), disj(Q51, Q52))), lit(trueo)) :- _oroEvaloEvalo(Q51, Q52).
oroEvaloEvalo(disj(lit(falso), conj(lit(trueo), conj(Q51, Q52))), lit(trueo)) :- _andoEvaloEvalo(Q51, Q52).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q53, Q54), lit(falso))), lit(trueo)) :- oroEvaloEvalo(Q53, Q54).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q53, Q54), disj(Q55, Q56))), lit(trueo)) :- oro(Q57, Q58), _evalo(Q53, Q57), _evalo(Q54, Q58), __oro(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q53, Q54), conj(Q55, Q56))), lit(trueo)) :- oro(Q57, Q58), _evalo(Q53, Q57), _evalo(Q54, Q58), __ando(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q53, Q54), lit(falso))), lit(trueo)) :- andoEvaloEvalo(Q53, Q54).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q53, Q54), disj(Q61, Q62))), lit(trueo)) :- _ando(Q57, Q58), _evalo(Q53, Q57), _evalo(Q54, Q58), __oro(Q63, Q64), _evalo(Q61, Q63), _evalo(Q62, Q64).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q53, Q54), conj(Q61, Q62))), lit(trueo)) :- _ando(Q57, Q58), _evalo(Q53, Q57), _evalo(Q54, Q58), __ando(Q63, Q64), _evalo(Q61, Q63), _evalo(Q62, Q64).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), lit(falso))), lit(trueo)).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), disj(Q65, Q66))), lit(trueo)) :- _oroEvaloEvalo(Q65, Q66).
oroEvaloEvalo(disj(lit(falso), conj(lit(falso), conj(Q65, Q66))), lit(trueo)) :- _andoEvaloEvalo(Q65, Q66).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q67, Q68), lit(falso))), lit(trueo)) :- _oroEvaloEvalo(Q67, Q68).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q67, Q68), disj(Q69, Q70))), lit(trueo)) :- __oro(Q71, Q72), _evalo(Q67, Q71), _evalo(Q68, Q72), __oro(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74).
oroEvaloEvalo(disj(lit(falso), conj(disj(Q67, Q68), conj(Q69, Q70))), lit(trueo)) :- __oro(Q71, Q72), _evalo(Q67, Q71), _evalo(Q68, Q72), __ando(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q67, Q68), lit(falso))), lit(trueo)) :- _andoEvaloEvalo(Q67, Q68).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q67, Q68), disj(Q75, Q76))), lit(trueo)) :- __ando(Q71, Q72), _evalo(Q67, Q71), _evalo(Q68, Q72), __oro(Q77, Q78), _evalo(Q75, Q77), _evalo(Q76, Q78).
oroEvaloEvalo(disj(lit(falso), conj(conj(Q67, Q68), conj(Q75, Q76))), lit(trueo)) :- __ando(Q71, Q72), _evalo(Q67, Q71), _evalo(Q68, Q72), __ando(Q77, Q78), _evalo(Q75, Q77), _evalo(Q76, Q78).
oroEvaloEvalo(disj(var(Q81), disj(Q79, Q80)), lit(trueo)).
oroEvaloEvalo(disj(var(Q81), conj(Q79, Q80)), lit(trueo)).
oroEvaloEvalo(disj(disj(Q82, Q83), lit(falso)), lit(trueo)) :- _oroEvaloEvalo(Q82, Q83).
oroEvaloEvalo(disj(disj(Q82, Q83), var(Q84)), lit(trueo)).
oroEvaloEvalo(disj(disj(Q82, Q83), disj(Q85, Q86)), lit(trueo)) :- _oroEvaloEvaloOroEvaloEvalo(Q82, Q83, Q85, Q86).
oroEvaloEvalo(disj(disj(Q82, Q83), conj(Q85, Q86)), lit(trueo)) :- _oroEvaloEvaloAndoEvaloEvalo(Q82, Q83, Q85, Q86).
oroEvaloEvalo(disj(conj(Q82, Q83), lit(falso)), lit(trueo)) :- _andoEvaloEvalo(Q82, Q83).
oroEvaloEvalo(disj(conj(Q82, Q83), var(Q87)), lit(trueo)).
oroEvaloEvalo(disj(conj(Q82, Q83), disj(Q88, Q89)), lit(trueo)) :- _andoEvaloEvaloOroEvaloEvalo(Q82, Q83, Q88, Q89).
oroEvaloEvalo(disj(conj(Q82, Q83), conj(Q88, Q89)), lit(trueo)) :- _andoEvaloEvaloAndoEvaloEvalo(Q82, Q83, Q88, Q89).
oroEvaloEvalo(disj(Q30, Q31), var(Q90)).
oroEvaloEvalo(disj(Q30, Q31), disj(Q91, Q92)) :- __oroEvaloEvaloOroEvaloEvalo(Q30, Q31, Q91, Q92).
oroEvaloEvalo(disj(Q30, Q31), conj(Q91, Q92)) :- __oroEvaloEvaloAndoEvaloEvalo(Q30, Q31, Q91, Q92).
oroEvaloEvalo(conj(Q30, Q31), lit(trueo)) :- _andoEvaloEvalo(Q30, Q31).
oroEvaloEvalo(conj(Q30, Q31), var(Q93)).
oroEvaloEvalo(conj(Q30, Q31), disj(Q94, Q95)) :- __andoEvaloEvaloOroEvaloEvalo(Q30, Q31, Q94, Q95).
oroEvaloEvalo(conj(Q30, Q31), conj(Q94, Q95)) :- __andoEvaloEvaloAndoEvaloEvalo(Q30, Q31, Q94, Q95).
oroEvaloEvalo(lit(trueo), lit(falso)).
oroEvaloEvalo(lit(trueo), disj(Q96, Q97)) :- _oroEvaloEvalo(Q96, Q97).
oroEvaloEvalo(lit(trueo), conj(Q96, Q97)) :- _andoEvaloEvalo(Q96, Q97).
oroEvaloEvalo(var(Q100), disj(Q98, Q99)).
oroEvaloEvalo(var(Q100), conj(Q98, Q99)).
oroEvaloEvalo(disj(Q101, Q102), lit(falso)) :- oroEvaloEvalo(Q101, Q102).
oroEvaloEvalo(disj(Q101, Q102), var(Q103)).
oroEvaloEvalo(disj(Q101, Q102), disj(Q104, Q105)) :- ___oroEvaloEvaloOroEvaloEvalo(Q101, Q102, Q104, Q105).
oroEvaloEvalo(disj(Q101, Q102), conj(Q104, Q105)) :- ___oroEvaloEvaloAndoEvaloEvalo(Q101, Q102, Q104, Q105).
oroEvaloEvalo(conj(Q101, Q102), lit(falso)) :- andoEvaloEvalo(Q101, Q102).
oroEvaloEvalo(conj(Q101, Q102), var(Q106)).
oroEvaloEvalo(conj(Q101, Q102), disj(Q107, Q108)) :- ___andoEvaloEvaloOroEvaloEvalo(Q101, Q102, Q107, Q108).
oroEvaloEvalo(conj(Q101, Q102), conj(Q107, Q108)) :- ___andoEvaloEvaloAndoEvaloEvalo(Q101, Q102, Q107, Q108).
andoEvaloEvalo(lit(trueo), lit(trueo)).
andoEvaloEvalo(lit(trueo), disj(Q1, Q2)) :- oroEvaloEvalo(Q1, Q2).
andoEvaloEvalo(lit(trueo), conj(Q1, Q2)) :- andoEvaloEvalo(Q1, Q2).
andoEvaloEvalo(disj(Q3, Q4), lit(trueo)) :- oroEvaloEvalo(Q3, Q4).
andoEvaloEvalo(disj(Q3, Q4), disj(Q5, Q6)) :- oro(Q7, Q8), _evalo(Q3, Q7), _evalo(Q4, Q8), oro(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10).
andoEvaloEvalo(disj(Q3, Q4), conj(Q5, Q6)) :- oro(Q7, Q8), _evalo(Q3, Q7), _evalo(Q4, Q8), _ando(Q9, Q10), _evalo(Q5, Q9), _evalo(Q6, Q10).
andoEvaloEvalo(conj(Q3, Q4), lit(trueo)) :- andoEvaloEvalo(Q3, Q4).
andoEvaloEvalo(conj(Q3, Q4), disj(Q11, Q12)) :- _ando(Q7, Q8), _evalo(Q3, Q7), _evalo(Q4, Q8), oro(Q13, Q14), _evalo(Q11, Q13), _evalo(Q12, Q14).
andoEvaloEvalo(conj(Q3, Q4), conj(Q11, Q12)) :- _ando(Q7, Q8), _evalo(Q3, Q7), _evalo(Q4, Q8), _ando(Q13, Q14), _evalo(Q11, Q13), _evalo(Q12, Q14).
oro(trueo, trueo).
oro(falso, trueo).
oro(trueo, falso).
_evalo(lit(Y12), Y12).
_evalo(disj(Q1, Q2), Y12) :- _oro(Y12, Q3, Q4), _evalo(Q1, Q3), _evalo(Q2, Q4).
_evalo(conj(Q1, Q2), Y12) :- ando(Y12, Q3, Q4), _evalo(Q1, Q3), _evalo(Q2, Q4).
_oro(trueo, trueo, trueo).
_oro(trueo, falso, trueo).
_oro(trueo, trueo, falso).
_oro(falso, falso, falso).
ando(trueo, trueo, trueo).
ando(falso, falso, trueo).
ando(falso, trueo, falso).
ando(falso, falso, falso).
_ando(trueo, trueo).
oroEvaloEvaloOroEvaloEvalo(Y36, Y37, Y40, Y41) :- oro(Q1, Q2), _evalo(Y36, Q1), _evalo(Y37, Q2), oro(Q3, Q4), _evalo(Y40, Q3), _evalo(Y41, Q4).
oroEvaloEvaloAndoEvaloEvalo(Y44, Y45, Y48, Y49) :- oro(Q1, Q2), _evalo(Y44, Q1), _evalo(Y45, Q2), _ando(Q3, Q4), _evalo(Y48, Q3), _evalo(Y49, Q4).
andoEvaloEvaloOroEvaloEvalo(Y57, Y58, Y61, Y62) :- _ando(Q1, Q2), _evalo(Y57, Q1), _evalo(Y58, Q2), oro(Q3, Q4), _evalo(Y61, Q3), _evalo(Y62, Q4).
andoEvaloEvaloAndoEvaloEvalo(Y65, Y66, Y69, Y70) :- _ando(Q1, Q2), _evalo(Y65, Q1), _evalo(Y66, Q2), _ando(Q3, Q4), _evalo(Y69, Q3), _evalo(Y70, Q4).
_oroEvaloEvalo(lit(falso), lit(falso)).
_oroEvaloEvalo(lit(falso), disj(Q1, Q2)) :- _oroEvaloEvalo(Q1, Q2).
_oroEvaloEvalo(lit(falso), conj(lit(falso), lit(trueo))).
_oroEvaloEvalo(lit(falso), conj(lit(falso), disj(Q3, Q4))) :- oroEvaloEvalo(Q3, Q4).
_oroEvaloEvalo(lit(falso), conj(lit(falso), conj(Q3, Q4))) :- andoEvaloEvalo(Q3, Q4).
_oroEvaloEvalo(lit(falso), conj(var(Q7), disj(Q5, Q6))).
_oroEvaloEvalo(lit(falso), conj(disj(Q8, Q9), lit(trueo))) :- _oroEvaloEvalo(Q8, Q9).
_oroEvaloEvalo(lit(falso), conj(disj(Q8, Q9), disj(Q10, Q11))) :- __oro(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13), oro(Q14, Q15), _evalo(Q10, Q14), _evalo(Q11, Q15).
_oroEvaloEvalo(lit(falso), conj(disj(Q8, Q9), conj(Q10, Q11))) :- __oro(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13), _ando(Q14, Q15), _evalo(Q10, Q14), _evalo(Q11, Q15).
_oroEvaloEvalo(lit(falso), conj(conj(Q8, Q9), lit(trueo))) :- _andoEvaloEvalo(Q8, Q9).
_oroEvaloEvalo(lit(falso), conj(conj(Q8, Q9), disj(Q16, Q17))) :- __ando(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13), oro(Q18, Q19), _evalo(Q16, Q18), _evalo(Q17, Q19).
_oroEvaloEvalo(lit(falso), conj(conj(Q8, Q9), conj(Q16, Q17))) :- __ando(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13), _ando(Q18, Q19), _evalo(Q16, Q18), _evalo(Q17, Q19).
_oroEvaloEvalo(lit(falso), conj(lit(trueo), lit(falso))).
_oroEvaloEvalo(lit(falso), conj(lit(trueo), disj(Q20, Q21))) :- _oroEvaloEvalo(Q20, Q21).
_oroEvaloEvalo(lit(falso), conj(lit(trueo), conj(Q20, Q21))) :- _andoEvaloEvalo(Q20, Q21).
_oroEvaloEvalo(lit(falso), conj(disj(Q22, Q23), lit(falso))) :- oroEvaloEvalo(Q22, Q23).
_oroEvaloEvalo(lit(falso), conj(disj(Q22, Q23), disj(Q24, Q25))) :- oro(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27), __oro(Q28, Q29), _evalo(Q24, Q28), _evalo(Q25, Q29).
_oroEvaloEvalo(lit(falso), conj(disj(Q22, Q23), conj(Q24, Q25))) :- oro(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27), __ando(Q28, Q29), _evalo(Q24, Q28), _evalo(Q25, Q29).
_oroEvaloEvalo(lit(falso), conj(conj(Q22, Q23), lit(falso))) :- andoEvaloEvalo(Q22, Q23).
_oroEvaloEvalo(lit(falso), conj(conj(Q22, Q23), disj(Q30, Q31))) :- _ando(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27), __oro(Q32, Q33), _evalo(Q30, Q32), _evalo(Q31, Q33).
_oroEvaloEvalo(lit(falso), conj(conj(Q22, Q23), conj(Q30, Q31))) :- _ando(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27), __ando(Q32, Q33), _evalo(Q30, Q32), _evalo(Q31, Q33).
_oroEvaloEvalo(lit(falso), conj(lit(falso), lit(falso))).
_oroEvaloEvalo(lit(falso), conj(lit(falso), disj(Q34, Q35))) :- _oroEvaloEvalo(Q34, Q35).
_oroEvaloEvalo(lit(falso), conj(lit(falso), conj(Q34, Q35))) :- _andoEvaloEvalo(Q34, Q35).
_oroEvaloEvalo(lit(falso), conj(disj(Q36, Q37), lit(falso))) :- _oroEvaloEvalo(Q36, Q37).
_oroEvaloEvalo(lit(falso), conj(disj(Q36, Q37), disj(Q38, Q39))) :- __oro(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41), __oro(Q42, Q43), _evalo(Q38, Q42), _evalo(Q39, Q43).
_oroEvaloEvalo(lit(falso), conj(disj(Q36, Q37), conj(Q38, Q39))) :- __oro(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41), __ando(Q42, Q43), _evalo(Q38, Q42), _evalo(Q39, Q43).
_oroEvaloEvalo(lit(falso), conj(conj(Q36, Q37), lit(falso))) :- _andoEvaloEvalo(Q36, Q37).
_oroEvaloEvalo(lit(falso), conj(conj(Q36, Q37), disj(Q44, Q45))) :- __ando(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41), __oro(Q46, Q47), _evalo(Q44, Q46), _evalo(Q45, Q47).
_oroEvaloEvalo(lit(falso), conj(conj(Q36, Q37), conj(Q44, Q45))) :- __ando(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41), __ando(Q46, Q47), _evalo(Q44, Q46), _evalo(Q45, Q47).
_oroEvaloEvalo(var(Q50), disj(Q48, Q49)).
_oroEvaloEvalo(var(Q50), conj(Q48, Q49)).
_oroEvaloEvalo(disj(Q51, Q52), lit(falso)) :- _oroEvaloEvalo(Q51, Q52).
_oroEvaloEvalo(disj(Q51, Q52), var(Q53)).
_oroEvaloEvalo(disj(Q51, Q52), disj(Q54, Q55)) :- _oroEvaloEvaloOroEvaloEvalo(Q51, Q52, Q54, Q55).
_oroEvaloEvalo(disj(Q51, Q52), conj(Q54, Q55)) :- _oroEvaloEvaloAndoEvaloEvalo(Q51, Q52, Q54, Q55).
_oroEvaloEvalo(conj(Q51, Q52), lit(falso)) :- _andoEvaloEvalo(Q51, Q52).
_oroEvaloEvalo(conj(Q51, Q52), var(Q56)).
_oroEvaloEvalo(conj(Q51, Q52), disj(Q57, Q58)) :- _andoEvaloEvaloOroEvaloEvalo(Q51, Q52, Q57, Q58).
_oroEvaloEvalo(conj(Q51, Q52), conj(Q57, Q58)) :- _andoEvaloEvaloAndoEvaloEvalo(Q51, Q52, Q57, Q58).
__oro(falso, falso).
_andoEvaloEvalo(lit(falso), lit(trueo)).
_andoEvaloEvalo(lit(falso), disj(Q1, Q2)) :- oroEvaloEvalo(Q1, Q2).
_andoEvaloEvalo(lit(falso), conj(Q1, Q2)) :- andoEvaloEvalo(Q1, Q2).
_andoEvaloEvalo(var(Q5), disj(Q3, Q4)).
_andoEvaloEvalo(disj(Q6, Q7), lit(trueo)) :- _oroEvaloEvalo(Q6, Q7).
_andoEvaloEvalo(disj(Q6, Q7), disj(Q8, Q9)) :- __oro(Q10, Q11), _evalo(Q6, Q10), _evalo(Q7, Q11), oro(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13).
_andoEvaloEvalo(disj(Q6, Q7), conj(Q8, Q9)) :- __oro(Q10, Q11), _evalo(Q6, Q10), _evalo(Q7, Q11), _ando(Q12, Q13), _evalo(Q8, Q12), _evalo(Q9, Q13).
_andoEvaloEvalo(conj(Q6, Q7), lit(trueo)) :- _andoEvaloEvalo(Q6, Q7).
_andoEvaloEvalo(conj(Q6, Q7), disj(Q14, Q15)) :- __ando(Q10, Q11), _evalo(Q6, Q10), _evalo(Q7, Q11), oro(Q16, Q17), _evalo(Q14, Q16), _evalo(Q15, Q17).
_andoEvaloEvalo(conj(Q6, Q7), conj(Q14, Q15)) :- __ando(Q10, Q11), _evalo(Q6, Q10), _evalo(Q7, Q11), _ando(Q16, Q17), _evalo(Q14, Q16), _evalo(Q15, Q17).
_andoEvaloEvalo(lit(trueo), lit(falso)).
_andoEvaloEvalo(lit(trueo), disj(Q18, Q19)) :- _oroEvaloEvalo(Q18, Q19).
_andoEvaloEvalo(lit(trueo), conj(Q18, Q19)) :- _andoEvaloEvalo(Q18, Q19).
_andoEvaloEvalo(disj(Q20, Q21), lit(falso)) :- oroEvaloEvalo(Q20, Q21).
_andoEvaloEvalo(disj(Q20, Q21), disj(Q22, Q23)) :- oro(Q24, Q25), _evalo(Q20, Q24), _evalo(Q21, Q25), __oro(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27).
_andoEvaloEvalo(disj(Q20, Q21), conj(Q22, Q23)) :- oro(Q24, Q25), _evalo(Q20, Q24), _evalo(Q21, Q25), __ando(Q26, Q27), _evalo(Q22, Q26), _evalo(Q23, Q27).
_andoEvaloEvalo(conj(Q20, Q21), lit(falso)) :- andoEvaloEvalo(Q20, Q21).
_andoEvaloEvalo(conj(Q20, Q21), disj(Q28, Q29)) :- _ando(Q24, Q25), _evalo(Q20, Q24), _evalo(Q21, Q25), __oro(Q30, Q31), _evalo(Q28, Q30), _evalo(Q29, Q31).
_andoEvaloEvalo(conj(Q20, Q21), conj(Q28, Q29)) :- _ando(Q24, Q25), _evalo(Q20, Q24), _evalo(Q21, Q25), __ando(Q30, Q31), _evalo(Q28, Q30), _evalo(Q29, Q31).
_andoEvaloEvalo(lit(falso), lit(falso)).
_andoEvaloEvalo(lit(falso), disj(Q32, Q33)) :- _oroEvaloEvalo(Q32, Q33).
_andoEvaloEvalo(lit(falso), conj(Q32, Q33)) :- _andoEvaloEvalo(Q32, Q33).
_andoEvaloEvalo(disj(Q34, Q35), lit(falso)) :- _oroEvaloEvalo(Q34, Q35).
_andoEvaloEvalo(disj(Q34, Q35), disj(Q36, Q37)) :- __oro(Q38, Q39), _evalo(Q34, Q38), _evalo(Q35, Q39), __oro(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41).
_andoEvaloEvalo(disj(Q34, Q35), conj(Q36, Q37)) :- __oro(Q38, Q39), _evalo(Q34, Q38), _evalo(Q35, Q39), __ando(Q40, Q41), _evalo(Q36, Q40), _evalo(Q37, Q41).
_andoEvaloEvalo(conj(Q34, Q35), lit(falso)) :- _andoEvaloEvalo(Q34, Q35).
_andoEvaloEvalo(conj(Q34, Q35), disj(Q42, Q43)) :- __ando(Q38, Q39), _evalo(Q34, Q38), _evalo(Q35, Q39), __oro(Q44, Q45), _evalo(Q42, Q44), _evalo(Q43, Q45).
_andoEvaloEvalo(conj(Q34, Q35), conj(Q42, Q43)) :- __ando(Q38, Q39), _evalo(Q34, Q38), _evalo(Q35, Q39), __ando(Q44, Q45), _evalo(Q42, Q44), _evalo(Q43, Q45).
__ando(falso, trueo).
__ando(trueo, falso).
__ando(falso, falso).
__evalo(lit(trueo)).
__evalo(disj(lit(trueo), lit(trueo))).
__evalo(disj(lit(trueo), disj(Q3, Q4))) :- oroEvaloEvalo(Q3, Q4).
__evalo(disj(lit(trueo), conj(lit(trueo), lit(trueo)))).
__evalo(disj(lit(trueo), conj(lit(trueo), disj(Q5, Q6)))) :- oroEvaloEvalo(Q5, Q6).
__evalo(disj(lit(trueo), conj(lit(trueo), conj(Q5, Q6)))) :- andoEvaloEvalo(Q5, Q6).
__evalo(disj(lit(trueo), conj(disj(Q7, Q8), lit(trueo)))) :- oroEvaloEvalo(Q7, Q8).
__evalo(disj(lit(trueo), conj(disj(Q7, Q8), disj(Q9, Q10)))) :- oro(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12), oro(Q13, Q14), _evalo(Q9, Q13), _evalo(Q10, Q14).
__evalo(disj(lit(trueo), conj(disj(Q7, Q8), conj(Q9, Q10)))) :- oro(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12), _ando(Q13, Q14), _evalo(Q9, Q13), _evalo(Q10, Q14).
__evalo(disj(lit(trueo), conj(conj(Q7, Q8), lit(trueo)))) :- andoEvaloEvalo(Q7, Q8).
__evalo(disj(lit(trueo), conj(conj(Q7, Q8), disj(Q15, Q16)))) :- _ando(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12), oro(Q17, Q18), _evalo(Q15, Q17), _evalo(Q16, Q18).
__evalo(disj(lit(trueo), conj(conj(Q7, Q8), conj(Q15, Q16)))) :- _ando(Q11, Q12), _evalo(Q7, Q11), _evalo(Q8, Q12), _ando(Q17, Q18), _evalo(Q15, Q17), _evalo(Q16, Q18).
__evalo(disj(var(Q21), disj(Q19, Q20))).
__evalo(disj(var(Q21), conj(Q19, Q20))).
__evalo(disj(disj(Q22, Q23), lit(trueo))) :- oroEvaloEvalo(Q22, Q23).
__evalo(disj(disj(Q22, Q23), var(Q24))).
__evalo(disj(disj(Q22, Q23), disj(Q25, Q26))) :- oroEvaloEvaloOroEvaloEvalo(Q22, Q23, Q25, Q26).
__evalo(disj(disj(Q22, Q23), conj(Q25, Q26))) :- oroEvaloEvaloAndoEvaloEvalo(Q22, Q23, Q25, Q26).
__evalo(disj(conj(Q22, Q23), lit(trueo))) :- andoEvaloEvalo(Q22, Q23).
__evalo(disj(conj(Q22, Q23), var(Q27))).
__evalo(disj(conj(Q22, Q23), disj(Q28, Q29))) :- andoEvaloEvaloOroEvaloEvalo(Q22, Q23, Q28, Q29).
__evalo(disj(conj(Q22, Q23), conj(Q28, Q29))) :- andoEvaloEvaloAndoEvaloEvalo(Q22, Q23, Q28, Q29).
__evalo(disj(lit(falso), lit(trueo))).
__evalo(disj(lit(falso), disj(Q30, Q31))) :- oroEvaloEvalo(Q30, Q31).
__evalo(disj(lit(falso), conj(Q30, Q31))) :- andoEvaloEvalo(Q30, Q31).
__evalo(disj(disj(lit(falso), lit(falso)), lit(trueo))).
__evalo(disj(disj(lit(falso), disj(Q34, Q35)), lit(trueo))) :- _oroEvaloEvalo(Q34, Q35).
__evalo(disj(disj(lit(falso), conj(lit(falso), lit(trueo))), lit(trueo))).
__evalo(disj(disj(lit(falso), conj(lit(falso), disj(Q36, Q37))), lit(trueo))) :- oroEvaloEvalo(Q36, Q37).
__evalo(disj(disj(lit(falso), conj(lit(falso), conj(Q36, Q37))), lit(trueo))) :- andoEvaloEvalo(Q36, Q37).
__evalo(disj(disj(lit(falso), conj(var(Q40), disj(Q38, Q39))), lit(trueo))).
__evalo(disj(disj(lit(falso), conj(disj(Q41, Q42), lit(trueo))), lit(trueo))) :- _oroEvaloEvalo(Q41, Q42).
__evalo(disj(disj(lit(falso), conj(disj(Q41, Q42), disj(Q43, Q44))), lit(trueo))) :- __oro(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46), oro(Q47, Q48), _evalo(Q43, Q47), _evalo(Q44, Q48).
__evalo(disj(disj(lit(falso), conj(disj(Q41, Q42), conj(Q43, Q44))), lit(trueo))) :- __oro(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46), _ando(Q47, Q48), _evalo(Q43, Q47), _evalo(Q44, Q48).
__evalo(disj(disj(lit(falso), conj(conj(Q41, Q42), lit(trueo))), lit(trueo))) :- _andoEvaloEvalo(Q41, Q42).
__evalo(disj(disj(lit(falso), conj(conj(Q41, Q42), disj(Q49, Q50))), lit(trueo))) :- __ando(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46), oro(Q51, Q52), _evalo(Q49, Q51), _evalo(Q50, Q52).
__evalo(disj(disj(lit(falso), conj(conj(Q41, Q42), conj(Q49, Q50))), lit(trueo))) :- __ando(Q45, Q46), _evalo(Q41, Q45), _evalo(Q42, Q46), _ando(Q51, Q52), _evalo(Q49, Q51), _evalo(Q50, Q52).
__evalo(disj(disj(lit(falso), conj(lit(trueo), lit(falso))), lit(trueo))).
__evalo(disj(disj(lit(falso), conj(lit(trueo), disj(Q53, Q54))), lit(trueo))) :- _oroEvaloEvalo(Q53, Q54).
__evalo(disj(disj(lit(falso), conj(lit(trueo), conj(Q53, Q54))), lit(trueo))) :- _andoEvaloEvalo(Q53, Q54).
__evalo(disj(disj(lit(falso), conj(disj(Q55, Q56), lit(falso))), lit(trueo))) :- oroEvaloEvalo(Q55, Q56).
__evalo(disj(disj(lit(falso), conj(disj(Q55, Q56), disj(Q57, Q58))), lit(trueo))) :- oro(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60), __oro(Q61, Q62), _evalo(Q57, Q61), _evalo(Q58, Q62).
__evalo(disj(disj(lit(falso), conj(disj(Q55, Q56), conj(Q57, Q58))), lit(trueo))) :- oro(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60), __ando(Q61, Q62), _evalo(Q57, Q61), _evalo(Q58, Q62).
__evalo(disj(disj(lit(falso), conj(conj(Q55, Q56), lit(falso))), lit(trueo))) :- andoEvaloEvalo(Q55, Q56).
__evalo(disj(disj(lit(falso), conj(conj(Q55, Q56), disj(Q63, Q64))), lit(trueo))) :- _ando(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60), __oro(Q65, Q66), _evalo(Q63, Q65), _evalo(Q64, Q66).
__evalo(disj(disj(lit(falso), conj(conj(Q55, Q56), conj(Q63, Q64))), lit(trueo))) :- _ando(Q59, Q60), _evalo(Q55, Q59), _evalo(Q56, Q60), __ando(Q65, Q66), _evalo(Q63, Q65), _evalo(Q64, Q66).
__evalo(disj(disj(lit(falso), conj(lit(falso), lit(falso))), lit(trueo))).
__evalo(disj(disj(lit(falso), conj(lit(falso), disj(Q67, Q68))), lit(trueo))) :- _oroEvaloEvalo(Q67, Q68).
__evalo(disj(disj(lit(falso), conj(lit(falso), conj(Q67, Q68))), lit(trueo))) :- _andoEvaloEvalo(Q67, Q68).
__evalo(disj(disj(lit(falso), conj(disj(Q69, Q70), lit(falso))), lit(trueo))) :- _oroEvaloEvalo(Q69, Q70).
__evalo(disj(disj(lit(falso), conj(disj(Q69, Q70), disj(Q71, Q72))), lit(trueo))) :- __oro(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74), __oro(Q75, Q76), _evalo(Q71, Q75), _evalo(Q72, Q76).
__evalo(disj(disj(lit(falso), conj(disj(Q69, Q70), conj(Q71, Q72))), lit(trueo))) :- __oro(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74), __ando(Q75, Q76), _evalo(Q71, Q75), _evalo(Q72, Q76).
__evalo(disj(disj(lit(falso), conj(conj(Q69, Q70), lit(falso))), lit(trueo))) :- _andoEvaloEvalo(Q69, Q70).
__evalo(disj(disj(lit(falso), conj(conj(Q69, Q70), disj(Q77, Q78))), lit(trueo))) :- __ando(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74), __oro(Q79, Q80), _evalo(Q77, Q79), _evalo(Q78, Q80).
__evalo(disj(disj(lit(falso), conj(conj(Q69, Q70), conj(Q77, Q78))), lit(trueo))) :- __ando(Q73, Q74), _evalo(Q69, Q73), _evalo(Q70, Q74), __ando(Q79, Q80), _evalo(Q77, Q79), _evalo(Q78, Q80).
__evalo(disj(disj(var(Q83), disj(Q81, Q82)), lit(trueo))).
__evalo(disj(disj(var(Q83), conj(Q81, Q82)), lit(trueo))).
__evalo(disj(disj(disj(Q84, Q85), lit(falso)), lit(trueo))) :- _oroEvaloEvalo(Q84, Q85).
__evalo(disj(disj(disj(Q84, Q85), var(Q86)), lit(trueo))).
__evalo(disj(disj(disj(Q84, Q85), disj(Q87, Q88)), lit(trueo))) :- _oroEvaloEvaloOroEvaloEvalo(Q84, Q85, Q87, Q88).
__evalo(disj(disj(disj(Q84, Q85), conj(Q87, Q88)), lit(trueo))) :- _oroEvaloEvaloAndoEvaloEvalo(Q84, Q85, Q87, Q88).
__evalo(disj(disj(conj(Q84, Q85), lit(falso)), lit(trueo))) :- _andoEvaloEvalo(Q84, Q85).
__evalo(disj(disj(conj(Q84, Q85), var(Q89)), lit(trueo))).
__evalo(disj(disj(conj(Q84, Q85), disj(Q90, Q91)), lit(trueo))) :- _andoEvaloEvaloOroEvaloEvalo(Q84, Q85, Q90, Q91).
__evalo(disj(disj(conj(Q84, Q85), conj(Q90, Q91)), lit(trueo))) :- _andoEvaloEvaloAndoEvaloEvalo(Q84, Q85, Q90, Q91).
__evalo(disj(disj(Q32, Q33), var(Q92))).
__evalo(disj(disj(Q32, Q33), disj(Q93, Q94))) :- __oroEvaloEvaloOroEvaloEvalo(Q32, Q33, Q93, Q94).
__evalo(disj(disj(Q32, Q33), conj(Q93, Q94))) :- __oroEvaloEvaloAndoEvaloEvalo(Q32, Q33, Q93, Q94).
__evalo(disj(conj(Q32, Q33), lit(trueo))) :- _andoEvaloEvalo(Q32, Q33).
__evalo(disj(conj(Q32, Q33), var(Q95))).
__evalo(disj(conj(Q32, Q33), disj(Q96, Q97))) :- __andoEvaloEvaloOroEvaloEvalo(Q32, Q33, Q96, Q97).
__evalo(disj(conj(Q32, Q33), conj(Q96, Q97))) :- __andoEvaloEvaloAndoEvaloEvalo(Q32, Q33, Q96, Q97).
__evalo(disj(lit(trueo), lit(falso))).
__evalo(disj(lit(trueo), disj(Q98, Q99))) :- _oroEvaloEvalo(Q98, Q99).
__evalo(disj(lit(trueo), conj(Q98, Q99))) :- _andoEvaloEvalo(Q98, Q99).
__evalo(disj(var(Q102), disj(Q100, Q101))).
__evalo(disj(var(Q102), conj(Q100, Q101))).
__evalo(disj(disj(Q103, Q104), lit(falso))) :- oroEvaloEvalo(Q103, Q104).
__evalo(disj(disj(Q103, Q104), var(Q105))).
__evalo(disj(disj(Q103, Q104), disj(Q106, Q107))) :- ___oroEvaloEvaloOroEvaloEvalo(Q103, Q104, Q106, Q107).
__evalo(disj(disj(Q103, Q104), conj(Q106, Q107))) :- ___oroEvaloEvaloAndoEvaloEvalo(Q103, Q104, Q106, Q107).
__evalo(disj(conj(Q103, Q104), lit(falso))) :- andoEvaloEvalo(Q103, Q104).
__evalo(disj(conj(Q103, Q104), var(Q108))).
__evalo(disj(conj(Q103, Q104), disj(Q109, Q110))) :- ___andoEvaloEvaloOroEvaloEvalo(Q103, Q104, Q109, Q110).
__evalo(disj(conj(Q103, Q104), conj(Q109, Q110))) :- ___andoEvaloEvaloAndoEvaloEvalo(Q103, Q104, Q109, Q110).
__evalo(conj(Q1, Q2)) :- andoEvaloEvalo(Q1, Q2).
_oroEvaloEvaloOroEvaloEvalo(Y106, Y107, Y110, Y111) :- __oro(Q1, Q2), _evalo(Y106, Q1), _evalo(Y107, Q2), __oro(Q3, Q4), _evalo(Y110, Q3), _evalo(Y111, Q4).
_oroEvaloEvaloAndoEvaloEvalo(Y114, Y115, Y118, Y119) :- __oro(Q1, Q2), _evalo(Y114, Q1), _evalo(Y115, Q2), __ando(Q3, Q4), _evalo(Y118, Q3), _evalo(Y119, Q4).
_andoEvaloEvaloOroEvaloEvalo(Y127, Y128, Y131, Y132) :- __ando(Q1, Q2), _evalo(Y127, Q1), _evalo(Y128, Q2), __oro(Q3, Q4), _evalo(Y131, Q3), _evalo(Y132, Q4).
_andoEvaloEvaloAndoEvaloEvalo(Y135, Y136, Y139, Y140) :- __ando(Q1, Q2), _evalo(Y135, Q1), _evalo(Y136, Q2), __ando(Q3, Q4), _evalo(Y139, Q3), _evalo(Y140, Q4).
__oroEvaloEvaloOroEvaloEvalo(Y148, Y149, Y152, Y153) :- __oro(Q1, Q2), _evalo(Y148, Q1), _evalo(Y149, Q2), oro(Q3, Q4), _evalo(Y152, Q3), _evalo(Y153, Q4).
__oroEvaloEvaloAndoEvaloEvalo(Y156, Y157, Y160, Y161) :- __oro(Q1, Q2), _evalo(Y156, Q1), _evalo(Y157, Q2), _ando(Q3, Q4), _evalo(Y160, Q3), _evalo(Y161, Q4).
__andoEvaloEvaloOroEvaloEvalo(Y169, Y170, Y173, Y174) :- __ando(Q1, Q2), _evalo(Y169, Q1), _evalo(Y170, Q2), oro(Q3, Q4), _evalo(Y173, Q3), _evalo(Y174, Q4).
__andoEvaloEvaloAndoEvaloEvalo(Y177, Y178, Y181, Y182) :- __ando(Q1, Q2), _evalo(Y177, Q1), _evalo(Y178, Q2), _ando(Q3, Q4), _evalo(Y181, Q3), _evalo(Y182, Q4).
___oroEvaloEvaloOroEvaloEvalo(Y200, Y201, Y204, Y205) :- oro(Q1, Q2), _evalo(Y200, Q1), _evalo(Y201, Q2), __oro(Q3, Q4), _evalo(Y204, Q3), _evalo(Y205, Q4).
___oroEvaloEvaloAndoEvaloEvalo(Y208, Y209, Y212, Y213) :- oro(Q1, Q2), _evalo(Y208, Q1), _evalo(Y209, Q2), __ando(Q3, Q4), _evalo(Y212, Q3), _evalo(Y213, Q4).
___andoEvaloEvaloOroEvaloEvalo(Y221, Y222, Y225, Y226) :- _ando(Q1, Q2), _evalo(Y221, Q1), _evalo(Y222, Q2), __oro(Q3, Q4), _evalo(Y225, Q3), _evalo(Y226, Q4).
___andoEvaloEvaloAndoEvaloEvalo(Y229, Y230, Y233, Y234) :- _ando(Q1, Q2), _evalo(Y229, Q1), _evalo(Y230, Q2), __ando(Q3, Q4), _evalo(Y233, Q3), _evalo(Y234, Q4).