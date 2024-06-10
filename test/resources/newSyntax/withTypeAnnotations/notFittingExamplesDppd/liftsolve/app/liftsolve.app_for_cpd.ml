open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 = 
  let rec solve z1 z2 = ((fresh (fRules) (((z1 === fRules) &&& (z2 === (List.nil ()))))) ||| (fresh (fRules fNgh fNgt fNgbody) (((z1 === fRules) &&& ((z2 === (fNgh % fNgt)) &&& ((non_ground_member ((term ((clause ())) ((fNgh % fNgbody)))) fRules) &&& ((solve fRules fNgbody) &&& (solve fRules fNgt)))))))) 
  and non_ground_member z1 z2 = ((fresh (fNgx fGrh fGrt) (((z1 === fNgx) &&& ((z2 === (fGrh % fGrt)) &&& (non_ground_member fNgx fGrt))))) ||| (fresh (fNgx fGrh fGrt) (((z1 === fNgx) &&& ((z2 === (fGrh % fGrt)) &&& (make_non_ground fGrh fNgx)))))) 
  and make_non_ground z1 z2 = (fresh (fG fNg fSub) (((z1 === fG) &&& ((z2 === fNg) &&& (mkng fG fNg ((List.nil ())) fSub))))) 
  and neq z1 z2 = ((fresh (fT) (((z1 === (zero ())) &&& (z2 === (succ fT))))) ||| ((fresh (fT) (((z1 === (succ fT)) &&& (z2 === (zero ()))))) ||| (fresh (fT1 fT2) (((z1 === (succ fT1)) &&& ((z2 === (succ fT2)) &&& (neq fT1 fT2))))))) 
  and mkng z1 z2 z3 z4 = ((fresh (fN fTerm2) (((z1 === (var fN)) &&& ((z2 === fTerm2) &&& ((z3 === (List.nil ())) &&& (z4 === (((sub fN fTerm2)) % ((List.nil ()))))))))) ||| ((fresh (fN fTerm2 fT) (((z1 === (var fN)) &&& ((z2 === fTerm2) &&& ((z3 === (((sub fN fTerm2)) % fT)) &&& (z4 === (((sub fN fTerm2)) % fT))))))) ||| ((fresh (fN fTerm2 fM fY fT fT1) (((z1 === (var fN)) &&& ((z2 === fTerm2) &&& ((z3 === (((sub fM fY)) % fT)) &&& ((z4 === (((sub fM fY)) % fT1)) &&& ((neq fN fM) &&& (mkng ((var fN)) fTerm2 fT fT1)))))))) ||| (fresh (fF fArgs fIArgs fInSub fOutSub) (((z1 === (term fF fArgs)) &&& ((z2 === (term fF fIArgs)) &&& ((z3 === fInSub) &&& ((z4 === fOutSub) &&& (l_mkng fArgs fIArgs fInSub fOutSub)))))))))) 
  and l_mkng z1 z2 z3 z4 = ((fresh (fOutSub) (((z1 === (List.nil ())) &&& ((z2 === (List.nil ())) &&& ((z3 === fOutSub) &&& (z4 === fOutSub)))))) ||| (fresh (fH fT fIh fIt fInSub fOutSub fIntSub) (((z1 === (fH % fT)) &&& ((z2 === (fIh % fIt)) &&& ((z3 === fInSub) &&& ((z4 === fOutSub) &&& ((mkng fH fIh fInSub fIntSub) &&& (l_mkng fT fIt fIntSub fOutSub))))))))) 
  in       (solve y1 y2)
