open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec oroEvaloEvalo y1 y2 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40 q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54 q55 q56 q57 q58 q59 q60 q61 q62 q63 q64 q65 q66 q67 q68 q69 q70 q71 q72 q73 q74 q75 q76 q77 q78 q79 q80 q81 q82 q83 q84 q85 q86 q87 q88 q89 q90 q91 q92 q93 q94 q95 q96 q97 q98 q99 q100 q101 q102 q103 q104 q105 q106 q107 q108) ((((y2 === (lit !!true)) &&& (y1 === (lit !!true))) ||| ((y2 === (disj q1 q2)) &&& (y1 === (lit !!true)) &&& (oroEvaloEvalo q1 q2)) ||| ((y2 === (conj q1 q2)) &&& (y1 === (lit !!true)) &&& (((q2 === (lit !!true)) &&& (q1 === (lit !!true))) ||| ((q2 === (disj q3 q4)) &&& (q1 === (lit !!true)) &&& (oroEvaloEvalo q3 q4)) ||| ((q2 === (conj q3 q4)) &&& (q1 === (lit !!true)) &&& (andoEvaloEvalo q3 q4)) ||| ((q2 === (lit !!true)) &&& (q1 === (disj q5 q6)) &&& (oroEvaloEvalo q5 q6)) ||| ((q2 === (disj q7 q8)) &&& (q1 === (disj q5 q6)) &&& (oro q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10) &&& (oro q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12)) ||| ((q2 === (conj q7 q8)) &&& (q1 === (disj q5 q6)) &&& (oro q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10) &&& (_ando q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12)) ||| ((q2 === (lit !!true)) &&& (q1 === (conj q5 q6)) &&& (andoEvaloEvalo q5 q6)) ||| ((q2 === (disj q13 q14)) &&& (q1 === (conj q5 q6)) &&& (_ando q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10) &&& (oro q15 q16) &&& (_evalo q13 q15) &&& (_evalo q14 q16)) ||| ((q2 === (conj q13 q14)) &&& (q1 === (conj q5 q6)) &&& (_ando q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10) &&& (_ando q15 q16) &&& (_evalo q13 q15) &&& (_evalo q14 q16)))) ||| ((y2 === (disj q17 q18)) &&& (y1 === (var q19))) ||| ((y2 === (conj q17 q18)) &&& (y1 === (var q19))) ||| ((y2 === (lit !!true)) &&& (y1 === (disj q20 q21)) &&& (oroEvaloEvalo q20 q21)) ||| ((y2 === (var q22)) &&& (y1 === (disj q20 q21))) ||| ((y2 === (disj q23 q24)) &&& (y1 === (disj q20 q21)) &&& (oroEvaloEvaloOroEvaloEvalo q20 q21 q23 q24)) ||| ((y2 === (conj q23 q24)) &&& (y1 === (disj q20 q21)) &&& (oroEvaloEvaloAndoEvaloEvalo q20 q21 q23 q24)) ||| ((y2 === (lit !!true)) &&& (y1 === (conj q20 q21)) &&& (andoEvaloEvalo q20 q21)) ||| ((y2 === (var q25)) &&& (y1 === (conj q20 q21))) ||| ((y2 === (disj q26 q27)) &&& (y1 === (conj q20 q21)) &&& (andoEvaloEvaloOroEvaloEvalo q20 q21 q26 q27)) ||| ((y2 === (conj q26 q27)) &&& (y1 === (conj q20 q21)) &&& (andoEvaloEvaloAndoEvaloEvalo q20 q21 q26 q27)) ||| ((y2 === (lit !!true)) &&& (y1 === (lit !!false))) ||| ((y2 === (disj q28 q29)) &&& (y1 === (lit !!false)) &&& (oroEvaloEvalo q28 q29)) ||| ((y2 === (conj q28 q29)) &&& (y1 === (lit !!false)) &&& (andoEvaloEvalo q28 q29)) ||| ((y2 === (lit !!true)) &&& (y1 === (disj q30 q31)) &&& (((q31 === (lit !!false)) &&& (q30 === (lit !!false))) ||| ((q31 === (disj q32 q33)) &&& (q30 === (lit !!false)) &&& (_oroEvaloEvalo q32 q33)) ||| ((q31 === (conj q32 q33)) &&& (q30 === (lit !!false)) &&& (((q33 === (lit !!true)) &&& (q32 === (lit !!false))) ||| ((q33 === (disj q34 q35)) &&& (q32 === (lit !!false)) &&& (oroEvaloEvalo q34 q35)) ||| ((q33 === (conj q34 q35)) &&& (q32 === (lit !!false)) &&& (andoEvaloEvalo q34 q35)) ||| ((q33 === (disj q36 q37)) &&& (q32 === (var q38))) ||| ((q33 === (lit !!true)) &&& (q32 === (disj q39 q40)) &&& (_oroEvaloEvalo q39 q40)) ||| ((q33 === (disj q41 q42)) &&& (q32 === (disj q39 q40)) &&& (__oro q43 q44) &&& (_evalo q39 q43) &&& (_evalo q40 q44) &&& (oro q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46)) ||| ((q33 === (conj q41 q42)) &&& (q32 === (disj q39 q40)) &&& (__oro q43 q44) &&& (_evalo q39 q43) &&& (_evalo q40 q44) &&& (_ando q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46)) ||| ((q33 === (lit !!true)) &&& (q32 === (conj q39 q40)) &&& (_andoEvaloEvalo q39 q40)) ||| ((q33 === (disj q47 q48)) &&& (q32 === (conj q39 q40)) &&& (__ando q43 q44) &&& (_evalo q39 q43) &&& (_evalo q40 q44) &&& (oro q49 q50) &&& (_evalo q47 q49) &&& (_evalo q48 q50)) ||| ((q33 === (conj q47 q48)) &&& (q32 === (conj q39 q40)) &&& (__ando q43 q44) &&& (_evalo q39 q43) &&& (_evalo q40 q44) &&& (_ando q49 q50) &&& (_evalo q47 q49) &&& (_evalo q48 q50)) ||| ((q33 === (lit !!false)) &&& (q32 === (lit !!true))) ||| ((q33 === (disj q51 q52)) &&& (q32 === (lit !!true)) &&& (_oroEvaloEvalo q51 q52)) ||| ((q33 === (conj q51 q52)) &&& (q32 === (lit !!true)) &&& (_andoEvaloEvalo q51 q52)) ||| ((q33 === (lit !!false)) &&& (q32 === (disj q53 q54)) &&& (oroEvaloEvalo q53 q54)) ||| ((q33 === (disj q55 q56)) &&& (q32 === (disj q53 q54)) &&& (oro q57 q58) &&& (_evalo q53 q57) &&& (_evalo q54 q58) &&& (__oro q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60)) ||| ((q33 === (conj q55 q56)) &&& (q32 === (disj q53 q54)) &&& (oro q57 q58) &&& (_evalo q53 q57) &&& (_evalo q54 q58) &&& (__ando q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60)) ||| ((q33 === (lit !!false)) &&& (q32 === (conj q53 q54)) &&& (andoEvaloEvalo q53 q54)) ||| ((q33 === (disj q61 q62)) &&& (q32 === (conj q53 q54)) &&& (_ando q57 q58) &&& (_evalo q53 q57) &&& (_evalo q54 q58) &&& (__oro q63 q64) &&& (_evalo q61 q63) &&& (_evalo q62 q64)) ||| ((q33 === (conj q61 q62)) &&& (q32 === (conj q53 q54)) &&& (_ando q57 q58) &&& (_evalo q53 q57) &&& (_evalo q54 q58) &&& (__ando q63 q64) &&& (_evalo q61 q63) &&& (_evalo q62 q64)) ||| ((q33 === (lit !!false)) &&& (q32 === (lit !!false))) ||| ((q33 === (disj q65 q66)) &&& (q32 === (lit !!false)) &&& (_oroEvaloEvalo q65 q66)) ||| ((q33 === (conj q65 q66)) &&& (q32 === (lit !!false)) &&& (_andoEvaloEvalo q65 q66)) ||| ((q33 === (lit !!false)) &&& (q32 === (disj q67 q68)) &&& (_oroEvaloEvalo q67 q68)) ||| ((q33 === (disj q69 q70)) &&& (q32 === (disj q67 q68)) &&& (__oro q71 q72) &&& (_evalo q67 q71) &&& (_evalo q68 q72) &&& (__oro q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74)) ||| ((q33 === (conj q69 q70)) &&& (q32 === (disj q67 q68)) &&& (__oro q71 q72) &&& (_evalo q67 q71) &&& (_evalo q68 q72) &&& (__ando q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74)) ||| ((q33 === (lit !!false)) &&& (q32 === (conj q67 q68)) &&& (_andoEvaloEvalo q67 q68)) ||| ((q33 === (disj q75 q76)) &&& (q32 === (conj q67 q68)) &&& (__ando q71 q72) &&& (_evalo q67 q71) &&& (_evalo q68 q72) &&& (__oro q77 q78) &&& (_evalo q75 q77) &&& (_evalo q76 q78)) ||| ((q33 === (conj q75 q76)) &&& (q32 === (conj q67 q68)) &&& (__ando q71 q72) &&& (_evalo q67 q71) &&& (_evalo q68 q72) &&& (__ando q77 q78) &&& (_evalo q75 q77) &&& (_evalo q76 q78)))) ||| ((q31 === (disj q79 q80)) &&& (q30 === (var q81))) ||| ((q31 === (conj q79 q80)) &&& (q30 === (var q81))) ||| ((q31 === (lit !!false)) &&& (q30 === (disj q82 q83)) &&& (_oroEvaloEvalo q82 q83)) ||| ((q31 === (var q84)) &&& (q30 === (disj q82 q83))) ||| ((q31 === (disj q85 q86)) &&& (q30 === (disj q82 q83)) &&& (_oroEvaloEvaloOroEvaloEvalo q82 q83 q85 q86)) ||| ((q31 === (conj q85 q86)) &&& (q30 === (disj q82 q83)) &&& (_oroEvaloEvaloAndoEvaloEvalo q82 q83 q85 q86)) ||| ((q31 === (lit !!false)) &&& (q30 === (conj q82 q83)) &&& (_andoEvaloEvalo q82 q83)) ||| ((q31 === (var q87)) &&& (q30 === (conj q82 q83))) ||| ((q31 === (disj q88 q89)) &&& (q30 === (conj q82 q83)) &&& (_andoEvaloEvaloOroEvaloEvalo q82 q83 q88 q89)) ||| ((q31 === (conj q88 q89)) &&& (q30 === (conj q82 q83)) &&& (_andoEvaloEvaloAndoEvaloEvalo q82 q83 q88 q89)))) ||| ((y2 === (var q90)) &&& (y1 === (disj q30 q31))) ||| ((y2 === (disj q91 q92)) &&& (y1 === (disj q30 q31)) &&& (__oroEvaloEvaloOroEvaloEvalo q30 q31 q91 q92)) ||| ((y2 === (conj q91 q92)) &&& (y1 === (disj q30 q31)) &&& (__oroEvaloEvaloAndoEvaloEvalo q30 q31 q91 q92)) ||| ((y2 === (lit !!true)) &&& (y1 === (conj q30 q31)) &&& (_andoEvaloEvalo q30 q31)) ||| ((y2 === (var q93)) &&& (y1 === (conj q30 q31))) ||| ((y2 === (disj q94 q95)) &&& (y1 === (conj q30 q31)) &&& (__andoEvaloEvaloOroEvaloEvalo q30 q31 q94 q95)) ||| ((y2 === (conj q94 q95)) &&& (y1 === (conj q30 q31)) &&& (__andoEvaloEvaloAndoEvaloEvalo q30 q31 q94 q95)) ||| ((y2 === (lit !!false)) &&& (y1 === (lit !!true))) ||| ((y2 === (disj q96 q97)) &&& (y1 === (lit !!true)) &&& (_oroEvaloEvalo q96 q97)) ||| ((y2 === (conj q96 q97)) &&& (y1 === (lit !!true)) &&& (_andoEvaloEvalo q96 q97)) ||| ((y2 === (disj q98 q99)) &&& (y1 === (var q100))) ||| ((y2 === (conj q98 q99)) &&& (y1 === (var q100))) ||| ((y2 === (lit !!false)) &&& (y1 === (disj q101 q102)) &&& (oroEvaloEvalo q101 q102)) ||| ((y2 === (var q103)) &&& (y1 === (disj q101 q102))) ||| ((y2 === (disj q104 q105)) &&& (y1 === (disj q101 q102)) &&& (___oroEvaloEvaloOroEvaloEvalo q101 q102 q104 q105)) ||| ((y2 === (conj q104 q105)) &&& (y1 === (disj q101 q102)) &&& (___oroEvaloEvaloAndoEvaloEvalo q101 q102 q104 q105)) ||| ((y2 === (lit !!false)) &&& (y1 === (conj q101 q102)) &&& (andoEvaloEvalo q101 q102)) ||| ((y2 === (var q106)) &&& (y1 === (conj q101 q102))) ||| ((y2 === (disj q107 q108)) &&& (y1 === (conj q101 q102)) &&& (___andoEvaloEvaloOroEvaloEvalo q101 q102 q107 q108)) ||| ((y2 === (conj q107 q108)) &&& (y1 === (conj q101 q102)) &&& (___andoEvaloEvaloAndoEvaloEvalo q101 q102 q107 q108))))) 
  and andoEvaloEvalo y5 y6 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14) ((((y6 === (lit !!true)) &&& (y5 === (lit !!true))) ||| ((y6 === (disj q1 q2)) &&& (y5 === (lit !!true)) &&& (oroEvaloEvalo q1 q2)) ||| ((y6 === (conj q1 q2)) &&& (y5 === (lit !!true)) &&& (andoEvaloEvalo q1 q2)) ||| ((y6 === (lit !!true)) &&& (y5 === (disj q3 q4)) &&& (oroEvaloEvalo q3 q4)) ||| ((y6 === (disj q5 q6)) &&& (y5 === (disj q3 q4)) &&& (oro q7 q8) &&& (_evalo q3 q7) &&& (_evalo q4 q8) &&& (oro q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10)) ||| ((y6 === (conj q5 q6)) &&& (y5 === (disj q3 q4)) &&& (oro q7 q8) &&& (_evalo q3 q7) &&& (_evalo q4 q8) &&& (_ando q9 q10) &&& (_evalo q5 q9) &&& (_evalo q6 q10)) ||| ((y6 === (lit !!true)) &&& (y5 === (conj q3 q4)) &&& (andoEvaloEvalo q3 q4)) ||| ((y6 === (disj q11 q12)) &&& (y5 === (conj q3 q4)) &&& (_ando q7 q8) &&& (_evalo q3 q7) &&& (_evalo q4 q8) &&& (oro q13 q14) &&& (_evalo q11 q13) &&& (_evalo q12 q14)) ||| ((y6 === (conj q11 q12)) &&& (y5 === (conj q3 q4)) &&& (_ando q7 q8) &&& (_evalo q3 q7) &&& (_evalo q4 q8) &&& (_ando q13 q14) &&& (_evalo q11 q13) &&& (_evalo q12 q14))))) 
  and oro y9 y10 = (((y10 === !!true) &&& (y9 === !!true)) ||| ((y10 === !!true) &&& (y9 === !!false)) ||| ((y10 === !!false) &&& (y9 === !!true))) 
  and _evalo y11 y12 = (fresh (q1 q2 q3 q4) (((y11 === (lit y12)) ||| ((y11 === (disj q1 q2)) &&& (_oro y12 q3 q4) &&& (_evalo q1 q3) &&& (_evalo q2 q4)) ||| ((y11 === (conj q1 q2)) &&& (ando y12 q3 q4) &&& (_evalo q1 q3) &&& (_evalo q2 q4))))) 
  and _oro y13 y14 y15 = (((y15 === !!true) &&& (y14 === !!true) &&& (y13 === !!true)) ||| ((y15 === !!true) &&& (y14 === !!false) &&& (y13 === !!true)) ||| ((y15 === !!false) &&& (y14 === !!true) &&& (y13 === !!true)) ||| ((y15 === !!false) &&& (y14 === !!false) &&& (y13 === !!false))) 
  and ando y16 y17 y18 = (((y18 === !!true) &&& (y17 === !!true) &&& (y16 === !!true)) ||| ((y18 === !!true) &&& (y17 === !!false) &&& (y16 === !!false)) ||| ((y18 === !!false) &&& (y17 === !!true) &&& (y16 === !!false)) ||| ((y18 === !!false) &&& (y17 === !!false) &&& (y16 === !!false))) 
  and _ando y19 y20 = ((y20 === !!true) &&& (y19 === !!true)) 
  and oroEvaloEvaloOroEvaloEvalo y36 y37 y40 y41 = (fresh (q1 q2 q3 q4) (((oro q1 q2) &&& (_evalo y36 q1) &&& (_evalo y37 q2) &&& (oro q3 q4) &&& (_evalo y40 q3) &&& (_evalo y41 q4)))) 
  and oroEvaloEvaloAndoEvaloEvalo y44 y45 y48 y49 = (fresh (q1 q2 q3 q4) (((oro q1 q2) &&& (_evalo y44 q1) &&& (_evalo y45 q2) &&& (_ando q3 q4) &&& (_evalo y48 q3) &&& (_evalo y49 q4)))) 
  and andoEvaloEvaloOroEvaloEvalo y57 y58 y61 y62 = (fresh (q1 q2 q3 q4) (((_ando q1 q2) &&& (_evalo y57 q1) &&& (_evalo y58 q2) &&& (oro q3 q4) &&& (_evalo y61 q3) &&& (_evalo y62 q4)))) 
  and andoEvaloEvaloAndoEvaloEvalo y65 y66 y69 y70 = (fresh (q1 q2 q3 q4) (((_ando q1 q2) &&& (_evalo y65 q1) &&& (_evalo y66 q2) &&& (_ando q3 q4) &&& (_evalo y69 q3) &&& (_evalo y70 q4)))) 
  and _oroEvaloEvalo y73 y74 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40 q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54 q55 q56 q57 q58) ((((y74 === (lit !!false)) &&& (y73 === (lit !!false))) ||| ((y74 === (disj q1 q2)) &&& (y73 === (lit !!false)) &&& (_oroEvaloEvalo q1 q2)) ||| ((y74 === (conj q1 q2)) &&& (y73 === (lit !!false)) &&& (((q2 === (lit !!true)) &&& (q1 === (lit !!false))) ||| ((q2 === (disj q3 q4)) &&& (q1 === (lit !!false)) &&& (oroEvaloEvalo q3 q4)) ||| ((q2 === (conj q3 q4)) &&& (q1 === (lit !!false)) &&& (andoEvaloEvalo q3 q4)) ||| ((q2 === (disj q5 q6)) &&& (q1 === (var q7))) ||| ((q2 === (lit !!true)) &&& (q1 === (disj q8 q9)) &&& (_oroEvaloEvalo q8 q9)) ||| ((q2 === (disj q10 q11)) &&& (q1 === (disj q8 q9)) &&& (__oro q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13) &&& (oro q14 q15) &&& (_evalo q10 q14) &&& (_evalo q11 q15)) ||| ((q2 === (conj q10 q11)) &&& (q1 === (disj q8 q9)) &&& (__oro q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13) &&& (_ando q14 q15) &&& (_evalo q10 q14) &&& (_evalo q11 q15)) ||| ((q2 === (lit !!true)) &&& (q1 === (conj q8 q9)) &&& (_andoEvaloEvalo q8 q9)) ||| ((q2 === (disj q16 q17)) &&& (q1 === (conj q8 q9)) &&& (__ando q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13) &&& (oro q18 q19) &&& (_evalo q16 q18) &&& (_evalo q17 q19)) ||| ((q2 === (conj q16 q17)) &&& (q1 === (conj q8 q9)) &&& (__ando q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13) &&& (_ando q18 q19) &&& (_evalo q16 q18) &&& (_evalo q17 q19)) ||| ((q2 === (lit !!false)) &&& (q1 === (lit !!true))) ||| ((q2 === (disj q20 q21)) &&& (q1 === (lit !!true)) &&& (_oroEvaloEvalo q20 q21)) ||| ((q2 === (conj q20 q21)) &&& (q1 === (lit !!true)) &&& (_andoEvaloEvalo q20 q21)) ||| ((q2 === (lit !!false)) &&& (q1 === (disj q22 q23)) &&& (oroEvaloEvalo q22 q23)) ||| ((q2 === (disj q24 q25)) &&& (q1 === (disj q22 q23)) &&& (oro q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27) &&& (__oro q28 q29) &&& (_evalo q24 q28) &&& (_evalo q25 q29)) ||| ((q2 === (conj q24 q25)) &&& (q1 === (disj q22 q23)) &&& (oro q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27) &&& (__ando q28 q29) &&& (_evalo q24 q28) &&& (_evalo q25 q29)) ||| ((q2 === (lit !!false)) &&& (q1 === (conj q22 q23)) &&& (andoEvaloEvalo q22 q23)) ||| ((q2 === (disj q30 q31)) &&& (q1 === (conj q22 q23)) &&& (_ando q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27) &&& (__oro q32 q33) &&& (_evalo q30 q32) &&& (_evalo q31 q33)) ||| ((q2 === (conj q30 q31)) &&& (q1 === (conj q22 q23)) &&& (_ando q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27) &&& (__ando q32 q33) &&& (_evalo q30 q32) &&& (_evalo q31 q33)) ||| ((q2 === (lit !!false)) &&& (q1 === (lit !!false))) ||| ((q2 === (disj q34 q35)) &&& (q1 === (lit !!false)) &&& (_oroEvaloEvalo q34 q35)) ||| ((q2 === (conj q34 q35)) &&& (q1 === (lit !!false)) &&& (_andoEvaloEvalo q34 q35)) ||| ((q2 === (lit !!false)) &&& (q1 === (disj q36 q37)) &&& (_oroEvaloEvalo q36 q37)) ||| ((q2 === (disj q38 q39)) &&& (q1 === (disj q36 q37)) &&& (__oro q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41) &&& (__oro q42 q43) &&& (_evalo q38 q42) &&& (_evalo q39 q43)) ||| ((q2 === (conj q38 q39)) &&& (q1 === (disj q36 q37)) &&& (__oro q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41) &&& (__ando q42 q43) &&& (_evalo q38 q42) &&& (_evalo q39 q43)) ||| ((q2 === (lit !!false)) &&& (q1 === (conj q36 q37)) &&& (_andoEvaloEvalo q36 q37)) ||| ((q2 === (disj q44 q45)) &&& (q1 === (conj q36 q37)) &&& (__ando q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41) &&& (__oro q46 q47) &&& (_evalo q44 q46) &&& (_evalo q45 q47)) ||| ((q2 === (conj q44 q45)) &&& (q1 === (conj q36 q37)) &&& (__ando q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41) &&& (__ando q46 q47) &&& (_evalo q44 q46) &&& (_evalo q45 q47)))) ||| ((y74 === (disj q48 q49)) &&& (y73 === (var q50))) ||| ((y74 === (conj q48 q49)) &&& (y73 === (var q50))) ||| ((y74 === (lit !!false)) &&& (y73 === (disj q51 q52)) &&& (_oroEvaloEvalo q51 q52)) ||| ((y74 === (var q53)) &&& (y73 === (disj q51 q52))) ||| ((y74 === (disj q54 q55)) &&& (y73 === (disj q51 q52)) &&& (_oroEvaloEvaloOroEvaloEvalo q51 q52 q54 q55)) ||| ((y74 === (conj q54 q55)) &&& (y73 === (disj q51 q52)) &&& (_oroEvaloEvaloAndoEvaloEvalo q51 q52 q54 q55)) ||| ((y74 === (lit !!false)) &&& (y73 === (conj q51 q52)) &&& (_andoEvaloEvalo q51 q52)) ||| ((y74 === (var q56)) &&& (y73 === (conj q51 q52))) ||| ((y74 === (disj q57 q58)) &&& (y73 === (conj q51 q52)) &&& (_andoEvaloEvaloOroEvaloEvalo q51 q52 q57 q58)) ||| ((y74 === (conj q57 q58)) &&& (y73 === (conj q51 q52)) &&& (_andoEvaloEvaloAndoEvaloEvalo q51 q52 q57 q58))))) 
  and __oro y82 y83 = ((y83 === !!false) &&& (y82 === !!false)) 
  and _andoEvaloEvalo y84 y85 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40 q41 q42 q43 q44 q45) ((((y85 === (lit !!true)) &&& (y84 === (lit !!false))) ||| ((y85 === (disj q1 q2)) &&& (y84 === (lit !!false)) &&& (oroEvaloEvalo q1 q2)) ||| ((y85 === (conj q1 q2)) &&& (y84 === (lit !!false)) &&& (andoEvaloEvalo q1 q2)) ||| ((y85 === (disj q3 q4)) &&& (y84 === (var q5))) ||| ((y85 === (lit !!true)) &&& (y84 === (disj q6 q7)) &&& (_oroEvaloEvalo q6 q7)) ||| ((y85 === (disj q8 q9)) &&& (y84 === (disj q6 q7)) &&& (__oro q10 q11) &&& (_evalo q6 q10) &&& (_evalo q7 q11) &&& (oro q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13)) ||| ((y85 === (conj q8 q9)) &&& (y84 === (disj q6 q7)) &&& (__oro q10 q11) &&& (_evalo q6 q10) &&& (_evalo q7 q11) &&& (_ando q12 q13) &&& (_evalo q8 q12) &&& (_evalo q9 q13)) ||| ((y85 === (lit !!true)) &&& (y84 === (conj q6 q7)) &&& (_andoEvaloEvalo q6 q7)) ||| ((y85 === (disj q14 q15)) &&& (y84 === (conj q6 q7)) &&& (__ando q10 q11) &&& (_evalo q6 q10) &&& (_evalo q7 q11) &&& (oro q16 q17) &&& (_evalo q14 q16) &&& (_evalo q15 q17)) ||| ((y85 === (conj q14 q15)) &&& (y84 === (conj q6 q7)) &&& (__ando q10 q11) &&& (_evalo q6 q10) &&& (_evalo q7 q11) &&& (_ando q16 q17) &&& (_evalo q14 q16) &&& (_evalo q15 q17)) ||| ((y85 === (lit !!false)) &&& (y84 === (lit !!true))) ||| ((y85 === (disj q18 q19)) &&& (y84 === (lit !!true)) &&& (_oroEvaloEvalo q18 q19)) ||| ((y85 === (conj q18 q19)) &&& (y84 === (lit !!true)) &&& (_andoEvaloEvalo q18 q19)) ||| ((y85 === (lit !!false)) &&& (y84 === (disj q20 q21)) &&& (oroEvaloEvalo q20 q21)) ||| ((y85 === (disj q22 q23)) &&& (y84 === (disj q20 q21)) &&& (oro q24 q25) &&& (_evalo q20 q24) &&& (_evalo q21 q25) &&& (__oro q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27)) ||| ((y85 === (conj q22 q23)) &&& (y84 === (disj q20 q21)) &&& (oro q24 q25) &&& (_evalo q20 q24) &&& (_evalo q21 q25) &&& (__ando q26 q27) &&& (_evalo q22 q26) &&& (_evalo q23 q27)) ||| ((y85 === (lit !!false)) &&& (y84 === (conj q20 q21)) &&& (andoEvaloEvalo q20 q21)) ||| ((y85 === (disj q28 q29)) &&& (y84 === (conj q20 q21)) &&& (_ando q24 q25) &&& (_evalo q20 q24) &&& (_evalo q21 q25) &&& (__oro q30 q31) &&& (_evalo q28 q30) &&& (_evalo q29 q31)) ||| ((y85 === (conj q28 q29)) &&& (y84 === (conj q20 q21)) &&& (_ando q24 q25) &&& (_evalo q20 q24) &&& (_evalo q21 q25) &&& (__ando q30 q31) &&& (_evalo q28 q30) &&& (_evalo q29 q31)) ||| ((y85 === (lit !!false)) &&& (y84 === (lit !!false))) ||| ((y85 === (disj q32 q33)) &&& (y84 === (lit !!false)) &&& (_oroEvaloEvalo q32 q33)) ||| ((y85 === (conj q32 q33)) &&& (y84 === (lit !!false)) &&& (_andoEvaloEvalo q32 q33)) ||| ((y85 === (lit !!false)) &&& (y84 === (disj q34 q35)) &&& (_oroEvaloEvalo q34 q35)) ||| ((y85 === (disj q36 q37)) &&& (y84 === (disj q34 q35)) &&& (__oro q38 q39) &&& (_evalo q34 q38) &&& (_evalo q35 q39) &&& (__oro q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41)) ||| ((y85 === (conj q36 q37)) &&& (y84 === (disj q34 q35)) &&& (__oro q38 q39) &&& (_evalo q34 q38) &&& (_evalo q35 q39) &&& (__ando q40 q41) &&& (_evalo q36 q40) &&& (_evalo q37 q41)) ||| ((y85 === (lit !!false)) &&& (y84 === (conj q34 q35)) &&& (_andoEvaloEvalo q34 q35)) ||| ((y85 === (disj q42 q43)) &&& (y84 === (conj q34 q35)) &&& (__ando q38 q39) &&& (_evalo q34 q38) &&& (_evalo q35 q39) &&& (__oro q44 q45) &&& (_evalo q42 q44) &&& (_evalo q43 q45)) ||| ((y85 === (conj q42 q43)) &&& (y84 === (conj q34 q35)) &&& (__ando q38 q39) &&& (_evalo q34 q38) &&& (_evalo q35 q39) &&& (__ando q44 q45) &&& (_evalo q42 q44) &&& (_evalo q43 q45))))) 
  and __ando y88 y89 = (((y89 === !!true) &&& (y88 === !!false)) ||| ((y89 === !!false) &&& (y88 === !!true)) ||| ((y89 === !!false) &&& (y88 === !!false))) 
  and __evalo y90 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40 q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54 q55 q56 q57 q58 q59 q60 q61 q62 q63 q64 q65 q66 q67 q68 q69 q70 q71 q72 q73 q74 q75 q76 q77 q78 q79 q80 q81 q82 q83 q84 q85 q86 q87 q88 q89 q90 q91 q92 q93 q94 q95 q96 q97 q98 q99 q100 q101 q102 q103 q104 q105 q106 q107 q108 q109 q110) (((y90 === (lit !!true)) ||| ((y90 === (disj q1 q2)) &&& (((q2 === (lit !!true)) &&& (q1 === (lit !!true))) ||| ((q2 === (disj q3 q4)) &&& (q1 === (lit !!true)) &&& (oroEvaloEvalo q3 q4)) ||| ((q2 === (conj q3 q4)) &&& (q1 === (lit !!true)) &&& (((q4 === (lit !!true)) &&& (q3 === (lit !!true))) ||| ((q4 === (disj q5 q6)) &&& (q3 === (lit !!true)) &&& (oroEvaloEvalo q5 q6)) ||| ((q4 === (conj q5 q6)) &&& (q3 === (lit !!true)) &&& (andoEvaloEvalo q5 q6)) ||| ((q4 === (lit !!true)) &&& (q3 === (disj q7 q8)) &&& (oroEvaloEvalo q7 q8)) ||| ((q4 === (disj q9 q10)) &&& (q3 === (disj q7 q8)) &&& (oro q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12) &&& (oro q13 q14) &&& (_evalo q9 q13) &&& (_evalo q10 q14)) ||| ((q4 === (conj q9 q10)) &&& (q3 === (disj q7 q8)) &&& (oro q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12) &&& (_ando q13 q14) &&& (_evalo q9 q13) &&& (_evalo q10 q14)) ||| ((q4 === (lit !!true)) &&& (q3 === (conj q7 q8)) &&& (andoEvaloEvalo q7 q8)) ||| ((q4 === (disj q15 q16)) &&& (q3 === (conj q7 q8)) &&& (_ando q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12) &&& (oro q17 q18) &&& (_evalo q15 q17) &&& (_evalo q16 q18)) ||| ((q4 === (conj q15 q16)) &&& (q3 === (conj q7 q8)) &&& (_ando q11 q12) &&& (_evalo q7 q11) &&& (_evalo q8 q12) &&& (_ando q17 q18) &&& (_evalo q15 q17) &&& (_evalo q16 q18)))) ||| ((q2 === (disj q19 q20)) &&& (q1 === (var q21))) ||| ((q2 === (conj q19 q20)) &&& (q1 === (var q21))) ||| ((q2 === (lit !!true)) &&& (q1 === (disj q22 q23)) &&& (oroEvaloEvalo q22 q23)) ||| ((q2 === (var q24)) &&& (q1 === (disj q22 q23))) ||| ((q2 === (disj q25 q26)) &&& (q1 === (disj q22 q23)) &&& (oroEvaloEvaloOroEvaloEvalo q22 q23 q25 q26)) ||| ((q2 === (conj q25 q26)) &&& (q1 === (disj q22 q23)) &&& (oroEvaloEvaloAndoEvaloEvalo q22 q23 q25 q26)) ||| ((q2 === (lit !!true)) &&& (q1 === (conj q22 q23)) &&& (andoEvaloEvalo q22 q23)) ||| ((q2 === (var q27)) &&& (q1 === (conj q22 q23))) ||| ((q2 === (disj q28 q29)) &&& (q1 === (conj q22 q23)) &&& (andoEvaloEvaloOroEvaloEvalo q22 q23 q28 q29)) ||| ((q2 === (conj q28 q29)) &&& (q1 === (conj q22 q23)) &&& (andoEvaloEvaloAndoEvaloEvalo q22 q23 q28 q29)) ||| ((q2 === (lit !!true)) &&& (q1 === (lit !!false))) ||| ((q2 === (disj q30 q31)) &&& (q1 === (lit !!false)) &&& (oroEvaloEvalo q30 q31)) ||| ((q2 === (conj q30 q31)) &&& (q1 === (lit !!false)) &&& (andoEvaloEvalo q30 q31)) ||| ((q2 === (lit !!true)) &&& (q1 === (disj q32 q33)) &&& (((q33 === (lit !!false)) &&& (q32 === (lit !!false))) ||| ((q33 === (disj q34 q35)) &&& (q32 === (lit !!false)) &&& (_oroEvaloEvalo q34 q35)) ||| ((q33 === (conj q34 q35)) &&& (q32 === (lit !!false)) &&& (((q35 === (lit !!true)) &&& (q34 === (lit !!false))) ||| ((q35 === (disj q36 q37)) &&& (q34 === (lit !!false)) &&& (oroEvaloEvalo q36 q37)) ||| ((q35 === (conj q36 q37)) &&& (q34 === (lit !!false)) &&& (andoEvaloEvalo q36 q37)) ||| ((q35 === (disj q38 q39)) &&& (q34 === (var q40))) ||| ((q35 === (lit !!true)) &&& (q34 === (disj q41 q42)) &&& (_oroEvaloEvalo q41 q42)) ||| ((q35 === (disj q43 q44)) &&& (q34 === (disj q41 q42)) &&& (__oro q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46) &&& (oro q47 q48) &&& (_evalo q43 q47) &&& (_evalo q44 q48)) ||| ((q35 === (conj q43 q44)) &&& (q34 === (disj q41 q42)) &&& (__oro q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46) &&& (_ando q47 q48) &&& (_evalo q43 q47) &&& (_evalo q44 q48)) ||| ((q35 === (lit !!true)) &&& (q34 === (conj q41 q42)) &&& (_andoEvaloEvalo q41 q42)) ||| ((q35 === (disj q49 q50)) &&& (q34 === (conj q41 q42)) &&& (__ando q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46) &&& (oro q51 q52) &&& (_evalo q49 q51) &&& (_evalo q50 q52)) ||| ((q35 === (conj q49 q50)) &&& (q34 === (conj q41 q42)) &&& (__ando q45 q46) &&& (_evalo q41 q45) &&& (_evalo q42 q46) &&& (_ando q51 q52) &&& (_evalo q49 q51) &&& (_evalo q50 q52)) ||| ((q35 === (lit !!false)) &&& (q34 === (lit !!true))) ||| ((q35 === (disj q53 q54)) &&& (q34 === (lit !!true)) &&& (_oroEvaloEvalo q53 q54)) ||| ((q35 === (conj q53 q54)) &&& (q34 === (lit !!true)) &&& (_andoEvaloEvalo q53 q54)) ||| ((q35 === (lit !!false)) &&& (q34 === (disj q55 q56)) &&& (oroEvaloEvalo q55 q56)) ||| ((q35 === (disj q57 q58)) &&& (q34 === (disj q55 q56)) &&& (oro q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60) &&& (__oro q61 q62) &&& (_evalo q57 q61) &&& (_evalo q58 q62)) ||| ((q35 === (conj q57 q58)) &&& (q34 === (disj q55 q56)) &&& (oro q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60) &&& (__ando q61 q62) &&& (_evalo q57 q61) &&& (_evalo q58 q62)) ||| ((q35 === (lit !!false)) &&& (q34 === (conj q55 q56)) &&& (andoEvaloEvalo q55 q56)) ||| ((q35 === (disj q63 q64)) &&& (q34 === (conj q55 q56)) &&& (_ando q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60) &&& (__oro q65 q66) &&& (_evalo q63 q65) &&& (_evalo q64 q66)) ||| ((q35 === (conj q63 q64)) &&& (q34 === (conj q55 q56)) &&& (_ando q59 q60) &&& (_evalo q55 q59) &&& (_evalo q56 q60) &&& (__ando q65 q66) &&& (_evalo q63 q65) &&& (_evalo q64 q66)) ||| ((q35 === (lit !!false)) &&& (q34 === (lit !!false))) ||| ((q35 === (disj q67 q68)) &&& (q34 === (lit !!false)) &&& (_oroEvaloEvalo q67 q68)) ||| ((q35 === (conj q67 q68)) &&& (q34 === (lit !!false)) &&& (_andoEvaloEvalo q67 q68)) ||| ((q35 === (lit !!false)) &&& (q34 === (disj q69 q70)) &&& (_oroEvaloEvalo q69 q70)) ||| ((q35 === (disj q71 q72)) &&& (q34 === (disj q69 q70)) &&& (__oro q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74) &&& (__oro q75 q76) &&& (_evalo q71 q75) &&& (_evalo q72 q76)) ||| ((q35 === (conj q71 q72)) &&& (q34 === (disj q69 q70)) &&& (__oro q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74) &&& (__ando q75 q76) &&& (_evalo q71 q75) &&& (_evalo q72 q76)) ||| ((q35 === (lit !!false)) &&& (q34 === (conj q69 q70)) &&& (_andoEvaloEvalo q69 q70)) ||| ((q35 === (disj q77 q78)) &&& (q34 === (conj q69 q70)) &&& (__ando q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74) &&& (__oro q79 q80) &&& (_evalo q77 q79) &&& (_evalo q78 q80)) ||| ((q35 === (conj q77 q78)) &&& (q34 === (conj q69 q70)) &&& (__ando q73 q74) &&& (_evalo q69 q73) &&& (_evalo q70 q74) &&& (__ando q79 q80) &&& (_evalo q77 q79) &&& (_evalo q78 q80)))) ||| ((q33 === (disj q81 q82)) &&& (q32 === (var q83))) ||| ((q33 === (conj q81 q82)) &&& (q32 === (var q83))) ||| ((q33 === (lit !!false)) &&& (q32 === (disj q84 q85)) &&& (_oroEvaloEvalo q84 q85)) ||| ((q33 === (var q86)) &&& (q32 === (disj q84 q85))) ||| ((q33 === (disj q87 q88)) &&& (q32 === (disj q84 q85)) &&& (_oroEvaloEvaloOroEvaloEvalo q84 q85 q87 q88)) ||| ((q33 === (conj q87 q88)) &&& (q32 === (disj q84 q85)) &&& (_oroEvaloEvaloAndoEvaloEvalo q84 q85 q87 q88)) ||| ((q33 === (lit !!false)) &&& (q32 === (conj q84 q85)) &&& (_andoEvaloEvalo q84 q85)) ||| ((q33 === (var q89)) &&& (q32 === (conj q84 q85))) ||| ((q33 === (disj q90 q91)) &&& (q32 === (conj q84 q85)) &&& (_andoEvaloEvaloOroEvaloEvalo q84 q85 q90 q91)) ||| ((q33 === (conj q90 q91)) &&& (q32 === (conj q84 q85)) &&& (_andoEvaloEvaloAndoEvaloEvalo q84 q85 q90 q91)))) ||| ((q2 === (var q92)) &&& (q1 === (disj q32 q33))) ||| ((q2 === (disj q93 q94)) &&& (q1 === (disj q32 q33)) &&& (__oroEvaloEvaloOroEvaloEvalo q32 q33 q93 q94)) ||| ((q2 === (conj q93 q94)) &&& (q1 === (disj q32 q33)) &&& (__oroEvaloEvaloAndoEvaloEvalo q32 q33 q93 q94)) ||| ((q2 === (lit !!true)) &&& (q1 === (conj q32 q33)) &&& (_andoEvaloEvalo q32 q33)) ||| ((q2 === (var q95)) &&& (q1 === (conj q32 q33))) ||| ((q2 === (disj q96 q97)) &&& (q1 === (conj q32 q33)) &&& (__andoEvaloEvaloOroEvaloEvalo q32 q33 q96 q97)) ||| ((q2 === (conj q96 q97)) &&& (q1 === (conj q32 q33)) &&& (__andoEvaloEvaloAndoEvaloEvalo q32 q33 q96 q97)) ||| ((q2 === (lit !!false)) &&& (q1 === (lit !!true))) ||| ((q2 === (disj q98 q99)) &&& (q1 === (lit !!true)) &&& (_oroEvaloEvalo q98 q99)) ||| ((q2 === (conj q98 q99)) &&& (q1 === (lit !!true)) &&& (_andoEvaloEvalo q98 q99)) ||| ((q2 === (disj q100 q101)) &&& (q1 === (var q102))) ||| ((q2 === (conj q100 q101)) &&& (q1 === (var q102))) ||| ((q2 === (lit !!false)) &&& (q1 === (disj q103 q104)) &&& (oroEvaloEvalo q103 q104)) ||| ((q2 === (var q105)) &&& (q1 === (disj q103 q104))) ||| ((q2 === (disj q106 q107)) &&& (q1 === (disj q103 q104)) &&& (___oroEvaloEvaloOroEvaloEvalo q103 q104 q106 q107)) ||| ((q2 === (conj q106 q107)) &&& (q1 === (disj q103 q104)) &&& (___oroEvaloEvaloAndoEvaloEvalo q103 q104 q106 q107)) ||| ((q2 === (lit !!false)) &&& (q1 === (conj q103 q104)) &&& (andoEvaloEvalo q103 q104)) ||| ((q2 === (var q108)) &&& (q1 === (conj q103 q104))) ||| ((q2 === (disj q109 q110)) &&& (q1 === (conj q103 q104)) &&& (___andoEvaloEvaloOroEvaloEvalo q103 q104 q109 q110)) ||| ((q2 === (conj q109 q110)) &&& (q1 === (conj q103 q104)) &&& (___andoEvaloEvaloAndoEvaloEvalo q103 q104 q109 q110)))) ||| ((y90 === (conj q1 q2)) &&& (andoEvaloEvalo q1 q2))))) 
  and _oroEvaloEvaloOroEvaloEvalo y106 y107 y110 y111 = (fresh (q1 q2 q3 q4) (((__oro q1 q2) &&& (_evalo y106 q1) &&& (_evalo y107 q2) &&& (__oro q3 q4) &&& (_evalo y110 q3) &&& (_evalo y111 q4)))) 
  and _oroEvaloEvaloAndoEvaloEvalo y114 y115 y118 y119 = (fresh (q1 q2 q3 q4) (((__oro q1 q2) &&& (_evalo y114 q1) &&& (_evalo y115 q2) &&& (__ando q3 q4) &&& (_evalo y118 q3) &&& (_evalo y119 q4)))) 
  and _andoEvaloEvaloOroEvaloEvalo y127 y128 y131 y132 = (fresh (q1 q2 q3 q4) (((__ando q1 q2) &&& (_evalo y127 q1) &&& (_evalo y128 q2) &&& (__oro q3 q4) &&& (_evalo y131 q3) &&& (_evalo y132 q4)))) 
  and _andoEvaloEvaloAndoEvaloEvalo y135 y136 y139 y140 = (fresh (q1 q2 q3 q4) (((__ando q1 q2) &&& (_evalo y135 q1) &&& (_evalo y136 q2) &&& (__ando q3 q4) &&& (_evalo y139 q3) &&& (_evalo y140 q4)))) 
  and __oroEvaloEvaloOroEvaloEvalo y148 y149 y152 y153 = (fresh (q1 q2 q3 q4) (((__oro q1 q2) &&& (_evalo y148 q1) &&& (_evalo y149 q2) &&& (oro q3 q4) &&& (_evalo y152 q3) &&& (_evalo y153 q4)))) 
  and __oroEvaloEvaloAndoEvaloEvalo y156 y157 y160 y161 = (fresh (q1 q2 q3 q4) (((__oro q1 q2) &&& (_evalo y156 q1) &&& (_evalo y157 q2) &&& (_ando q3 q4) &&& (_evalo y160 q3) &&& (_evalo y161 q4)))) 
  and __andoEvaloEvaloOroEvaloEvalo y169 y170 y173 y174 = (fresh (q1 q2 q3 q4) (((__ando q1 q2) &&& (_evalo y169 q1) &&& (_evalo y170 q2) &&& (oro q3 q4) &&& (_evalo y173 q3) &&& (_evalo y174 q4)))) 
  and __andoEvaloEvaloAndoEvaloEvalo y177 y178 y181 y182 = (fresh (q1 q2 q3 q4) (((__ando q1 q2) &&& (_evalo y177 q1) &&& (_evalo y178 q2) &&& (_ando q3 q4) &&& (_evalo y181 q3) &&& (_evalo y182 q4)))) 
  and ___oroEvaloEvaloOroEvaloEvalo y200 y201 y204 y205 = (fresh (q1 q2 q3 q4) (((oro q1 q2) &&& (_evalo y200 q1) &&& (_evalo y201 q2) &&& (__oro q3 q4) &&& (_evalo y204 q3) &&& (_evalo y205 q4)))) 
  and ___oroEvaloEvaloAndoEvaloEvalo y208 y209 y212 y213 = (fresh (q1 q2 q3 q4) (((oro q1 q2) &&& (_evalo y208 q1) &&& (_evalo y209 q2) &&& (__ando q3 q4) &&& (_evalo y212 q3) &&& (_evalo y213 q4)))) 
  and ___andoEvaloEvaloOroEvaloEvalo y221 y222 y225 y226 = (fresh (q1 q2 q3 q4) (((_ando q1 q2) &&& (_evalo y221 q1) &&& (_evalo y222 q2) &&& (__oro q3 q4) &&& (_evalo y225 q3) &&& (_evalo y226 q4)))) 
  and ___andoEvaloEvaloAndoEvaloEvalo y229 y230 y233 y234 = (fresh (q1 q2 q3 q4) (((_ando q1 q2) &&& (_evalo y229 q1) &&& (_evalo y230 q2) &&& (__ando q3 q4) &&& (_evalo y233 q3) &&& (_evalo y234 q4)))) 
  in                             (__evalo x0)
