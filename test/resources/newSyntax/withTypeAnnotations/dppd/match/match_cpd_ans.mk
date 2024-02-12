matcho y0 = (y0 == [] | appendoAppendo y0);

appendoAppendo y1 = (appendo y1 | _appendoAppendo y1);

appendo y5 = (y5 == [S (S (O))] | y5 == []);

_appendoAppendo y7 = (_appendo y7 | __appendoAppendo y7);

_appendo y11 = (y11 == (S (S (O)) :: [S (O)]) | __appendo y11);

__appendo y13 = (y13 == [S (O)] | y13 == []);

__appendoAppendo y15 = (___appendo y15 | ___appendoAppendo y15);

___appendo y19 = (y19 == (S (S (O)) :: (S (O) :: [S (O)])) | ____appendo y19);

____appendo y21 = (y21 == (S (O) :: [S (O)]) | __appendo y21);

___appendoAppendo y23 = (_____appendo y23 | ____appendoAppendo y23);

_____appendo y27 = (y27 == (S (S (O)) :: (S (O) :: (S (O) :: [O]))) | ______appendo y27);

______appendo y29 = (y29 == (S (O) :: (S (O) :: [O])) | _______appendo y29);

_______appendo y31 = (y31 == (S (O) :: [O]) | ________appendo y31);

________appendo y33 = (y33 == [O] | y33 == []);

____appendoAppendo y35 = (_________appendo y35 | _____appendoAppendo y35);

_________appendo y39 = (y39 == (S (S (O)) :: (S (O) :: (S (O) :: (O :: [O])))) | __________appendo y39);

__________appendo y41 = (y41 == (S (O) :: (S (O) :: (O :: [O]))) | ___________appendo y41);

___________appendo y43 = (y43 == (S (O) :: (O :: [O])) | ____________appendo y43);

____________appendo y45 = (y45 == (O :: [O]) | ________appendo y45);

_____appendoAppendo y47 = (_____________appendo y47 | ______appendoAppendo y47);

_____________appendo y51 = (y51 == (S (S (O)) :: (S (O) :: (S (O) :: (O :: (O :: [S (S (O))]))))) | ______________appendo y51);

______________appendo y53 = (y53 == (S (O) :: (S (O) :: (O :: (O :: [S (S (O))])))) | _______________appendo y53);

_______________appendo y55 = (y55 == (S (O) :: (O :: (O :: [S (S (O))]))) | ________________appendo y55);

________________appendo y57 = (y57 == (O :: (O :: [S (S (O))])) | _________________appendo y57);

_________________appendo y59 = (y59 == (O :: [S (S (O))]) | appendo y59);

______appendoAppendo y61 = (__________________appendo y61 | ________________________appendo y61);

__________________appendo y65 = (y65 == (S (S (O)) :: (S (O) :: (S (O) :: (O :: (O :: (S (S (O)) :: [O])))))) | ___________________appendo y65);

___________________appendo y67 = (y67 == (S (O) :: (S (O) :: (O :: (O :: (S (S (O)) :: [O]))))) | ____________________appendo y67);

____________________appendo y69 = (y69 == (S (O) :: (O :: (O :: (S (S (O)) :: [O])))) | _____________________appendo y69);

_____________________appendo y71 = (y71 == (O :: (O :: (S (S (O)) :: [O]))) | ______________________appendo y71);

______________________appendo y73 = (y73 == (O :: (S (S (O)) :: [O])) | _______________________appendo y73);

_______________________appendo y75 = (y75 == (S (S (O)) :: [O]) | ________appendo y75);

________________________appendo y77 = (y77 == (S (S (O)) :: (S (O) :: (S (O) :: (O :: (O :: (S (S (O)) :: (O :: [S (O)]))))))) | _________________________appendo y77);

_________________________appendo y79 = (y79 == (S (O) :: (S (O) :: (O :: (O :: (S (S (O)) :: (O :: [S (O)])))))) | __________________________appendo y79);

__________________________appendo y81 = (y81 == (S (O) :: (O :: (O :: (S (S (O)) :: (O :: [S (O)]))))) | ___________________________appendo y81);

___________________________appendo y83 = (y83 == (O :: (O :: (S (S (O)) :: (O :: [S (O)])))) | ____________________________appendo y83);

____________________________appendo y85 = (y85 == (O :: (S (S (O)) :: (O :: [S (O)]))) | _____________________________appendo y85);

_____________________________appendo y87 = (y87 == (S (S (O)) :: (O :: [S (O)])) | ______________________________appendo y87);

______________________________appendo y89 = (y89 == (O :: [S (O)]) | __appendo y89);


? matcho x0