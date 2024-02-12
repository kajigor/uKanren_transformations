open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec matcho y0 = ((y0 === (List.nil ())) ||| (appendoAppendo y0)) 
  and appendoAppendo y1 = ((appendo y1) ||| (_appendoAppendo y1)) 
  and appendo y5 = ((y5 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))) ||| (y5 === (List.nil ()))) 
  and _appendoAppendo y7 = ((_appendo y7) ||| (__appendoAppendo y7)) 
  and _appendo y11 = ((y11 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))) ||| (__appendo y11)) 
  and __appendo y13 = ((y13 === (((Nat.succ Nat.zero)) % ((List.nil ())))) ||| (y13 === (List.nil ()))) 
  and __appendoAppendo y15 = ((___appendo y15) ||| (___appendoAppendo y15)) 
  and ___appendo y19 = ((y19 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))) ||| (____appendo y19)) 
  and ____appendo y21 = ((y21 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))) ||| (__appendo y21)) 
  and ___appendoAppendo y23 = ((_____appendo y23) ||| (____appendoAppendo y23)) 
  and _____appendo y27 = ((y27 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((List.nil ())))))))))) ||| (______appendo y27)) 
  and ______appendo y29 = ((y29 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((List.nil ())))))))) ||| (_______appendo y29)) 
  and _______appendo y31 = ((y31 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((List.nil ())))))) ||| (________appendo y31)) 
  and ________appendo y33 = ((y33 === (Nat.zero % ((List.nil ())))) ||| (y33 === (List.nil ()))) 
  and ____appendoAppendo y35 = ((_________appendo y35) ||| (_____appendoAppendo y35)) 
  and _________appendo y39 = ((y39 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))))))) ||| (__________appendo y39)) 
  and __________appendo y41 = ((y41 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))))) ||| (___________appendo y41)) 
  and ___________appendo y43 = ((y43 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))) ||| (____________appendo y43)) 
  and ____________appendo y45 = ((y45 === (Nat.zero % ((Nat.zero % ((List.nil ())))))) ||| (________appendo y45)) 
  and _____appendoAppendo y47 = ((_____________appendo y47) ||| (______appendoAppendo y47)) 
  and _____________appendo y51 = ((y51 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))))))))) ||| (______________appendo y51)) 
  and ______________appendo y53 = ((y53 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))))))) ||| (_______________appendo y53)) 
  and _______________appendo y55 = ((y55 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))))) ||| (________________appendo y55)) 
  and ________________appendo y57 = ((y57 === (Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))) ||| (_________________appendo y57)) 
  and _________________appendo y59 = ((y59 === (Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))) ||| (appendo y59)) 
  and ______appendoAppendo y61 = ((__________________appendo y61) ||| (________________________appendo y61)) 
  and __________________appendo y65 = ((y65 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))))))))))))) ||| (___________________appendo y65)) 
  and ___________________appendo y67 = ((y67 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))))))))))) ||| (____________________appendo y67)) 
  and ____________________appendo y69 = ((y69 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))))))))) ||| (_____________________appendo y69)) 
  and _____________________appendo y71 = ((y71 === (Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))))))) ||| (______________________appendo y71)) 
  and ______________________appendo y73 = ((y73 === (Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))))) ||| (_______________________appendo y73)) 
  and _______________________appendo y75 = ((y75 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((List.nil ())))))) ||| (________appendo y75)) 
  and ________________________appendo y77 = ((y77 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))))))))) ||| (_________________________appendo y77)) 
  and _________________________appendo y79 = ((y79 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))))))) ||| (__________________________appendo y79)) 
  and __________________________appendo y81 = ((y81 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))))) ||| (___________________________appendo y81)) 
  and ___________________________appendo y83 = ((y83 === (Nat.zero % ((Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))) ||| (____________________________appendo y83)) 
  and ____________________________appendo y85 = ((y85 === (Nat.zero % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))) ||| (_____________________________appendo y85)) 
  and _____________________________appendo y87 = ((y87 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))) ||| (______________________________appendo y87)) 
  and ______________________________appendo y89 = ((y89 === (Nat.zero % ((((Nat.succ Nat.zero)) % ((List.nil ())))))) ||| (__appendo y89)) 
  in                                        (matcho x0)
