filter (static dynamic static) 
 sub1sds v0 v2 v3 = ((v2 == O & v0 == O) | (fresh v4 in ((v0 == S v4 & Unfold subssd v4 v3 v2))));
filter (static static dynamic) 
 subssd v0 v1 v2 = ((v1 == O & v0 == v2) | (fresh v3 in ((v1 == S v3 & Unfold sub1sds v0 v2 v3))));
filter (static static static) 
 isFinishState4sss v2 v5 v6 = ((v5 == Trueo & v2 == Trueo) | (v2 == v6 & v5 == Falso));
filter (static dynamic static) 
 eqNat1sds v1 v2 v3 = ((v2 == Falso & v1 == O) | (fresh v4 in ((v1 == S v4 & Unfold eqNatssd v3 v4 v2))));
filter (static static dynamic) 
 eqNatssd v0 v1 v2 = ((v0 == O & Unfold eqNat0sd v1 v2) | (fresh v3 in ((v0 == S v3 & Unfold eqNat1sds v1 v2 v3))));
filter (static static static) 
 isFinishStatesss v0 v1 v2 = (fresh v3, v4, v5, v6 in ((v0 == Pair v3 v4 & Unfold eqNatssd v3 v1 v5 & Unfold eqNatssd v4 v1 v6 & Unfold isFinishState4sss v2 v5 v6)));
filter (static dynamic static) 
 greater1sds v1 v2 v3 = ((v2 == Trueo & v1 == O) | (fresh v4 in ((v1 == S v4 & Unfold greaterssd v3 v4 v2))));
filter (static static dynamic) 
 greaterssd v0 v1 v2 = ((v2 == Falso & v0 == O) | (fresh v3 in ((v0 == S v3 & Unfold greater1sds v1 v2 v3))));
filter (dynamic static dynamic) 
 eqNat1dsd v1 v2 v3 = ((v1 == O & v2 == Falso) | (fresh v4 in ((Memo eqNatdds v3 v4 v2 & v1 == S v4))));
filter (static dynamic dynamic) 
 eqNat1sdd v1 v2 v3 = ((v2 == Falso & v1 == O) | (fresh v4 in ((v1 == S v4 & Unfold eqNatdsd v3 v4 v2))));
filter (dynamic static) 
 eqNat0ds v1 v2 = ((v1 == O & v2 == Trueo) | (fresh v3 in ((v2 == Falso & v1 == S v3))));
filter (static dynamic) 
 eqNat0sd v1 v2 = ((v2 == Trueo & v1 == O) | (fresh v3 in ((v2 == Falso & v1 == S v3))));
filter (dynamic dynamic static) 
 eqNatdds v0 v1 v2 = ((v0 == O & Unfold eqNat0ds v1 v2) | (fresh v3 in ((Unfold eqNat1dsd v1 v2 v3 & v0 == S v3))));
filter (dynamic static dynamic) 
 eqNatdsd v0 v1 v2 = ((v0 == O & Unfold eqNat0sd v1 v2) | (fresh v3 in ((Unfold eqNat1sdd v1 v2 v3 & v0 == S v3))));
filter (static static static dynamic) 
 createStatesssd v0 v1 v2 v3 = ((fresh v15, v16 in ((v0 == Fst & v15 == v1 & v16 == v2 & v3 == Pair v15 v16))) | (fresh v17, v18 in ((v0 == Snd & v17 == v2 & v18 == v1 & v3 == Pair v17 v18))));
filter (dynamic static static static static) 
 doStep45dssss v2 v6 v8 v9 v11 = ((fresh v12 in ((v11 == Trueo & Unfold subssd v8 v9 v12 & Unfold createStatesssd v6 v12 v9 v2))) | (fresh v39 in ((v39 == O & v11 == Falso & Unfold createStatesssd v6 v39 v8 v2))));
filter (static static dynamic dynamic) 
 createStatessdd v0 v1 v2 v3 = ((fresh v15, v16 in ((v0 == Fst & v15 == v1 & v16 == v2 & v3 == Pair v15 v16))) | (fresh v18, v17 in ((v0 == Snd & v18 == v1 & v17 == v2 & v3 == Pair v17 v18))));
filter (static dynamic) 
 capacitiessd v0 v1 = ((fresh v22, v21, v20, v19, v23 in ((v22 == O & v21 == S v22 & v20 == S v21 & v19 == S v20 & v0 == Fst & v23 == v19 & v1 == S v23))) | (fresh v32, v31, v30, v29, v28, v27, v26, v25, v24, v33 in ((v32 == O & v31 == S v32 & v30 == S v31 & v29 == S v30 & v28 == S v29 & v27 == S v28 & v26 == S v27 & v25 == S v26 & v24 == S v25 & v0 == Snd & v33 == v24 & v1 == S v33))));
filter (static dynamic) 
 anotherBottlesd v0 v1 = ((v1 == Snd & v0 == Fst) | (v1 == Fst & v0 == Snd));
filter (static static dynamic) 
 addssd v0 v1 v2 = ((v0 == O & v1 == v2) | (fresh v14, v3 in ((v14 == S v1 & v0 == S v3 & Unfold addssd v3 v14 v2))));
filter (dynamic static static static static) 
 doStep4dssss v2 v3 v4 v5 v6 = 
 	((fresh v8, v7 in 
		((v5 == Fill & Unfold capacitiessd v6 v8 & Unfold createStatessdd v6 v8 v7 v2))) | 
	(fresh v38, v7 in ((v38 == O & v5 == Empt & Unfold createStatessdd v6 v38 v7 v2))) | 
	(fresh v8, v10, v9, v11 in 
		((v5 == Pour & Unfold addssd v3 v4 v8 & Unfold anotherBottlesd v6 v10 & 
		Unfold capacitiessd v10 v9 & Unfold greaterssd v8 v9 v11 & Unfold doStep45dssss v2 v6 v8 v9 v11))));
		
filter (static static static) 
 doStep3sss v3 v4 v6 = ((fresh v7 in ((v6 == Fst & v4 == v7))) | (fresh v7 in ((v6 == Snd & v3 == v7))));
filter (static static dynamic) 
 doStepssd v0 v1 v2 = (fresh v3, v4, v5, v6 in ((v0 == Pair v3 v4 & v1 == Pair v5 v6 & Unfold doStep3sss v3 v4 v6 & Unfold doStep4dssss v2 v3 v4 v5 v6)));
filter (dynamic) 
 checkStep35d v2 = ((fresh v10 in ((v10 == Trueo & v2 == Falso))) | (fresh v10 in ((v10 == Falso & v2 == Trueo))));
filter (static dynamic) 
 checkStep34sd v11 v12 = ((fresh v10 in ((v10 == Trueo & v11 == Trueo))) | (fresh v10 in ((v11 == Falso & v10 == v12))));
filter (dynamic dynamic) 
 capacitiesdd v0 v1 = ((fresh v22, v21, v20, v19, v23 in ((v0 == Fst & v22 == O & v21 == S v22 & v20 == S v21 & v19 == S v20 & v23 == v19 & v1 == S v23))) | (fresh v32, v31, v30, v29, v28, v27, v26, v25, v24, v33 in ((v0 == Snd & v32 == O & v31 == S v32 & v30 == S v31 & v29 == S v30 & v28 == S v29 & v27 == S v28 & v26 == S v27 & v25 == S v26 & v24 == S v25 & v33 == v24 & v1 == S v33))));
filter (dynamic static) 
 capacitiesds v0 v1 = ((fresh v22, v21, v20, v19, v23 in ((v0 == Fst & v22 == O & v21 == S v22 & v20 == S v21 & v19 == S v20 & v1 == S v23 & v23 == v19))) | (fresh v32, v31, v30, v29, v28, v27, v26, v25, v24, v33 in ((v0 == Snd & v32 == O & v31 == S v32 & v30 == S v31 & v29 == S v30 & v28 == S v29 & v27 == S v28 & v26 == S v27 & v25 == S v26 & v24 == S v25 & v1 == S v33 & v33 == v24))));
filter (dynamic static) 
 anotherBottleds v0 v1 = ((v0 == Fst & v1 == Snd) | (v0 == Snd & v1 == Fst));
filter (dynamic dynamic) 
 checkStep3dd v1 v2 = ((fresh v5, v34, v7 in ((v5 == Fill & v34 == O & Unfold eqNatdsd v7 v34 v2))) | (fresh v5, v6, v9, v7 in ((v5 == Empt & Unfold capacitiesdd v6 v9 & Unfold eqNatdsd v7 v9 v2))) | (fresh v5, v35, v36, v7, v11, v12, v8, v13, v9, v6, v37 in ((v5 == Pour & v35 == O & v36 == v5 & Unfold checkStep35d v2 & Unfold eqNatdsd v7 v35 v11 & Unfold checkStep34sd v11 v12 & Unfold eqNatdds v8 v13 v12 & Unfold capacitiesds v9 v13 & Unfold anotherBottleds v6 v9 & v37 == v6 & v1 == Pair v36 v37))));
filter (static static) 
 checkStep2ss v3 v4 = ((fresh v6, v8 in ((v6 == Fst & v4 == v8))) | (fresh v6, v8 in ((v6 == Snd & v3 == v8))));
filter (static static) 
 checkStep1ss v3 v4 = ((fresh v6, v7 in ((v6 == Fst & v3 == v7))) | (fresh v6, v7 in ((v6 == Snd & v4 == v7))));
filter (static dynamic dynamic) 
 checkStepsdd v0 v1 v2 = (fresh v3, v4 in ((v0 == Pair v3 v4 & Unfold checkStep1ss v3 v4 & Unfold checkStep2ss v3 v4 & Unfold checkStep3dd v1 v2)));
filter (dynamic static static dynamic dynamic) 
 checkAnswer'4sssss v0 v2 v3 v4 v6 = ((fresh v7, v5 in ((v6 == Trueo & Unfold doStepssd v0 v4 v7 & Memo checkAnswer'sdss v7 v5 v2 v3))) | (v6 == Falso & v3 == Falso));
filter (dynamic dynamic static static) 
 checkAnswer'sdss v0 v1 v2 v3 = ((Unfold isFinishStatesss v0 v2 v3 & v1 == []) | (fresh v4, v6, v44, v5 in ((Unfold checkStepsdd v0 v4 v6 & Unfold checkAnswer'4sssss v0 v2 v3 v4 v6 & v44 == v4 & v1 == (v44 :: v5)))));
filter (dynamic static static) 
 checkAnswerdss v0 v1 v2 = (fresh v40, v41, v42, v43, v3 in ((v40 == O & v41 == O & v42 == v40 & v43 == v41 & v3 == Pair v42 v43 & Unfold checkAnswer'sdss v3 v0 v1 v2)));

(fresh answer in (Memo checkAnswerdss answer (S (S (S (S (S (S (S O))))))) Trueo))