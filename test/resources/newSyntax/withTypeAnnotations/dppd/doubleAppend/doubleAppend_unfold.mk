double_appendo y0 y1 y2 = ((y2 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))])))))) & y1 == [] & y0 == []) | appendoAppendo y0 y1 y2);

appendoAppendo y3 y4 y5 = ((y5 == (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))]))))) & appendo y3 y4) | _appendoAppendo y3 y4 y5);

appendo y7 y8 = ((y8 == [S (O)] & y7 == []) | (y8 == [] & y7 == [S (O)]));

_appendoAppendo y9 y10 y11 = ((y11 == (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))])))) & _appendo y9 y10) | __appendoAppendo y9 y10 y11);

_appendo y13 y14 = (fresh q1 in (((y14 == (S (O) :: [S (S (O))]) & y13 == []) | (y13 == (S (O) :: q1) & __appendo y14 q1))));

__appendo y15 y16 = ((y16 == [] & y15 == [S (S (O))]) | (y16 == [S (S (O))] & y15 == []));

__appendoAppendo y17 y18 y19 = ((y19 == (O :: (O :: (S (O) :: [S (S (O))]))) & ___appendo y17 y18) | ___appendoAppendo y17 y18 y19);

___appendo y21 y22 = (fresh q1 in (((y22 == (S (O) :: (S (S (O)) :: [S (S (S (O)))])) & y21 == []) | (y21 == (S (O) :: q1) & ____appendo y22 q1))));

____appendo y23 y24 = (fresh q1 in (((y24 == [] & y23 == (S (S (O)) :: [S (S (S (O)))])) | (y24 == (S (S (O)) :: q1) & _____appendo y23 q1))));

_____appendo y25 y26 = ((y26 == [] & y25 == [S (S (S (O)))]) | (y26 == [S (S (S (O)))] & y25 == []));

___appendoAppendo y27 y28 y29 = ((y29 == (O :: (S (O) :: [S (S (O))])) & ______appendo y27 y28) | ____appendoAppendo y27 y28 y29);

______appendo y31 y32 = (fresh q1 in (((y32 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: [O]))) & y31 == []) | (y31 == (S (O) :: q1) & _______appendo y32 q1))));

_______appendo y33 y34 = (fresh q1 in (((y34 == [] & y33 == (S (S (O)) :: (S (S (S (O))) :: [O]))) | (y34 == (S (S (O)) :: q1) & ________appendo y33 q1))));

________appendo y35 y36 = (fresh q1 in (((y36 == [] & y35 == (S (S (S (O))) :: [O])) | (y36 == (S (S (S (O))) :: q1) & _________appendo y35 q1))));

_________appendo y37 y38 = ((y38 == [] & y37 == [O]) | (y38 == [O] & y37 == []));

____appendoAppendo y39 y40 y41 = ((y41 == (S (O) :: [S (S (O))]) & __________appendo y39 y40) | _____appendoAppendo y39 y40 y41);

__________appendo y43 y44 = (fresh q1 in (((y44 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: (O :: [O])))) & y43 == []) | (y43 == (S (O) :: q1) & ___________appendo y44 q1))));

___________appendo y45 y46 = (fresh q1 in (((y46 == [] & y45 == (S (S (O)) :: (S (S (S (O))) :: (O :: [O])))) | (y46 == (S (S (O)) :: q1) & ____________appendo y45 q1))));

____________appendo y47 y48 = (fresh q1 in (((y48 == [] & y47 == (S (S (S (O))) :: (O :: [O]))) | (y48 == (S (S (S (O))) :: q1) & _____________appendo y47 q1))));

_____________appendo y49 y50 = (fresh q1 in (((y50 == [] & y49 == (O :: [O])) | (y50 == (O :: q1) & _________appendo y49 q1))));

_____appendoAppendo y51 y52 y53 = ((y53 == [S (S (O))] & ______________appendo y51 y52) | (y53 == [] & ___________________appendo y51 y52));

______________appendo y55 y56 = (fresh q1 in (((y56 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: [S (O)]))))) & y55 == []) | (y55 == (S (O) :: q1) & _______________appendo y56 q1))));

_______________appendo y57 y58 = (fresh q1 in (((y58 == [] & y57 == (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: [S (O)]))))) | (y58 == (S (S (O)) :: q1) & ________________appendo y57 q1))));

________________appendo y59 y60 = (fresh q1 in (((y60 == [] & y59 == (S (S (S (O))) :: (O :: (O :: [S (O)])))) | (y60 == (S (S (S (O))) :: q1) & _________________appendo y59 q1))));

_________________appendo y61 y62 = (fresh q1 in (((y62 == [] & y61 == (O :: (O :: [S (O)]))) | (y62 == (O :: q1) & __________________appendo y61 q1))));

__________________appendo y63 y64 = (fresh q1 in (((y64 == [] & y63 == (O :: [S (O)])) | (y64 == (O :: q1) & appendo q1 y63))));

___________________appendo y65 y66 = (fresh q1 in (((y66 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))])))))) & y65 == []) | (y65 == (S (O) :: q1) & ____________________appendo y66 q1))));

____________________appendo y67 y68 = (fresh q1 in (((y68 == [] & y67 == (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))])))))) | (y68 == (S (S (O)) :: q1) & _____________________appendo y67 q1))));

_____________________appendo y69 y70 = (fresh q1 in (((y70 == [] & y69 == (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))]))))) | (y70 == (S (S (S (O))) :: q1) & ______________________appendo y69 q1))));

______________________appendo y71 y72 = (fresh q1 in (((y72 == [] & y71 == (O :: (O :: (S (O) :: [S (S (O))])))) | (y72 == (O :: q1) & _______________________appendo y71 q1))));

_______________________appendo y73 y74 = (fresh q1 in (((y74 == [] & y73 == (O :: (S (O) :: [S (S (O))]))) | (y74 == (O :: q1) & _appendo q1 y73))));


? double_appendo x0 x1 x2