filter (dynamic dynamic) 
 swapdd v0 v1 = (fresh v32, v3, v33, v2 in ((v32 == v3 & v33 == v2 & v0 == Pair v2 v3 & v1 == Pair v32 v33)));
filter (static dynamic dynamic static static static static static static) 
 step'0sddssssss v0 v2 v3 v4 v5 v6 v8 v9 v10 = 
 	((fresh v35, v34, v37, v36, v38, v39 in 
		((v3 == Empty & v35 == False & v34 == Quad v4 v5 v6 v35 & v37 == True & v36 == Quad v8 v9 v10 v37 & v38 == v34 & 
		v39 == v36 & v2 == Pair v38 v39 & Unfold safes v2))) | 
	(fresh v40, v41, v43, v44, v42, v46, v47, v45, v48, v49 in 
		((v3 == Goat & v40 == Goat & v41 == True & Unfold getsss v0 v40 v41 & v43 == False & v44 == False & v42 == Quad v43 v5 v6 v44 & 
			v46 == True & v47 == True & v45 == Quad v46 v9 v10 v47 & v48 == v42 & v49 == v45 & v2 == Pair v48 v49 & Unfold safes v2))) | 
	(fresh v50, v51, v53, v54, v52, v56, v57, v55, v58, v59 in 
		((v3 == Wolf & v50 == Wolf & v51 == True & Unfold getsss v0 v50 v51 & v53 == False & v54 == False & v52 == Quad v4 v53 v6 v54 & 
			v56 == True & v57 == True & v55 == Quad v9 v56 v10 v57 & v58 == v52 & v59 == v55 & v2 == Pair v58 v59 & Unfold safes v2))) | 
	(fresh v60, v61, v63, v64, v62, v66, v67, v65, v68, v69 in 
		((v3 == Cabbage & v60 == Cabbage & v61 == True & Unfold getsss v0 v60 v61 & v63 == False & v64 == False & v62 == Quad v4 v5 v63 v64 & 
			v66 == True & v67 == True & v65 == Quad v8 v9 v66 v67 & v68 == v62 & v69 == v65 & v2 == Pair v68 v69 & Unfold safes v2))));

filter (static) 
 safe'00s v0 = 
 	((fresh v20, v21, v22, v23 in 
		((v20 == Cabbage & v21 == True & Unfold getsss v0 v20 v21 & v22 == Wolf & v23 == True & Unfold getsss v0 v22 v23))) | 
	(fresh v24, v25, v26, v27 in 
		((v24 == Cabbage & v25 == False & Unfold getsss v0 v24 v25 & v26 == Wolf & v27 == False & Unfold getsss v0 v26 v27))));
filter (static) 
 safe'0s v0 = 
 	((fresh v18, v19 in ((v18 == Goat & v19 == False & Unfold getsss v0 v18 v19))) | 
	(fresh v28, v29 in ((Unfold safe'00s v0 & v28 == Goat & v29 == True & Unfold getsss v0 v28 v29))));
filter (static) 
 safe's v0 = 
 	((fresh v16, v17 in ((v16 == Man & v17 == True & Unfold getsss v0 v16 v17))) | 
	(fresh v30, v31 in ((Unfold safe'0s v0 & v30 == Man & v31 == False & Unfold getsss v0 v30 v31))));
filter (static) 
 safes v0 = (fresh v1, v2 in ((v0 == Pair v1 v2 & Unfold safe's v1 & Unfold safe's v2)));
filter (static static dynamic static static static static static static) 
 step'0ssdssssss v0 v2 v3 v4 v5 v6 v8 v9 v10 = 
	((fresh v35, v34, v37, v36, v38, v39 in 
 		((Unfold safes v2 & v3 == Empty & v35 == False & v34 == Quad v4 v5 v6 v35 & v37 == True & v36 == Quad v8 v9 v10 v37 & 
			v2 == Pair v38 v39 & v38 == v34 & v39 == v36))) | 
	(fresh v40, v41, v43, v44, v42, v46, v47, v45, v48, v49 in 
		((Unfold safes v2 & v3 == Goat & v40 == Goat & v41 == True & Unfold getsss v0 v40 v41 & v43 == False & v44 == False & 
		v42 == Quad v43 v5 v6 v44 & v46 == True & v47 == True & v45 == Quad v46 v9 v10 v47 & v2 == Pair v48 v49 & v48 == v42 & v49 == v45))) | 
	(fresh v50, v51, v53, v54, v52, v56, v57, v55, v58, v59 in 
		((Unfold safes v2 & v3 == Wolf & v50 == Wolf & v51 == True & Unfold getsss v0 v50 v51 & v53 == False & v54 == False & 
		v52 == Quad v4 v53 v6 v54 & v56 == True & v57 == True & v55 == Quad v9 v56 v10 v57 & v2 == Pair v58 v59 & v58 == v52 & v59 == v55))) | 
	(fresh v60, v61, v63, v64, v62, v66, v67, v65, v68, v69 in 
		((Unfold safes v2 & v3 == Cabbage & v60 == Cabbage & v61 == True & Unfold getsss v0 v60 v61 & v63 == False & v64 == False & 
		v62 == Quad v4 v5 v63 v64 & v66 == True & v67 == True & v65 == Quad v8 v9 v66 v67 & v2 == Pair v68 v69 & v68 == v62 & v69 == v65))));
filter (static static dynamic dynamic) 
 step'ssdd v0 v1 v2 v3 = 
 	(fresh v4, v5, v6, v7, v8, v9, v10, v11 in 
		((v0 == Quad v4 v5 v6 v7 & v1 == Quad v8 v9 v10 v11 & Unfold step'0sddssssss v0 v2 v3 v4 v5 v6 v8 v9 v10)));
filter (static static static dynamic) 
 step'sssd v0 v1 v2 v3 = 
 	(fresh v4, v5, v6, v7, v8, v9, v10, v11 in 
		((v0 == Quad v4 v5 v6 v7 & v1 == Quad v8 v9 v10 v11 & Unfold step'0ssdssssss v0 v2 v3 v4 v5 v6 v8 v9 v10)));
filter (static static static) 
 getsss v0 v1 v2 = 
 	((fresh v12, v3, v4, v5 in ((v0 == Quad v12 v3 v4 v5 & v12 == v2 & v1 == Goat))) | 
	(fresh v3, v13, v4, v5 in ((v0 == Quad v3 v13 v4 v5 & v13 == v2 & v1 == Wolf))) | 
	(fresh v3, v4, v14, v5 in ((v0 == Quad v3 v4 v14 v5 & v14 == v2 & v1 == Cabbage))) |
	 (fresh v3, v4, v5, v15 in ((v0 == Quad v3 v4 v5 v15 & v15 == v2 & v1 == Man))));
filter (dynamic dynamic static static) 
 step0ddss v1 v2 v3 v4 = 
 	((fresh v70, v71, v72, v73 in 
		((v70 == Man & v71 == True & Unfold getsss v3 v70 v71 & v72 == Man & v73 == False & Unfold getsss v4 v72 v73 & Unfold step'ssdd v3 v4 v2 v1))) | 
	(fresh v74, v75, v76, v77, v5 in ((v74 == Man & v75 == True & Unfold getsss v4 v74 v75 & v76 == Man & v77 == False & 
		Unfold getsss v3 v76 v77 & Unfold swapdd v5 v2 & Unfold step'sssd v4 v3 v5 v1))));
filter (static dynamic dynamic) 
 stepsdd v0 v1 v2 = (fresh v3, v4 in ((v0 == Pair v3 v4 & Unfold step0ddss v1 v2 v3 v4)));
filter (static dynamic static) 
 evalsds v0 v1 v2 = 
 	((v0 == v2 & v1 == []) | 
	 (fresh v3, v5, v78, v4, v79 in 
	 	((Unfold stepsdd v0 v3 v5 & v78 == v3 & Memo evalsds v5 v4 v2 & v79 == v4 & v1 == (v78 :: v79)))));

(fresh x in (Memo evalsds (Pair (Quad True True True True) (Quad False False False False)) x (Pair (Quad False False False False) (Quad True True True True))))