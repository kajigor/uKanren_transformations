boundedEvalBridges state moves state' =
  evalBridges state moves state' & 
  (fresh aboundedEvalBridges in
     totalTime moves aboundedEvalBridges & 
     leo aboundedEvalBridges (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zro))))))))))))))));

evalBridges state moves state' =
  moves == Nil & 
  state == state' | 
  (fresh aeval, beval, ceval in
     moves == Cons aeval beval & 
     (fresh astep_state, bstep_state in
        state == State astep_state bstep_state & 
        ((fresh aisTorch, bisTorch, cisTorch, disTorch in
            astep_state == Qua True aisTorch bisTorch cisTorch disTorch) & 
        (fresh anoTorch, bnoTorch, cnoTorch, dnoTorch in
           bstep_state == Qua False anoTorch bnoTorch cnoTorch dnoTorch) & 
        (fresh aapply_quas, bapply_quas in
           ceval == State aapply_quas bapply_quas & 
           (fresh aapply_people, bapply_people in
              aeval == Move aapply_people bapply_people & 
              (fresh achainMove, bchainMove in
                 (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                    bstep_state == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                    achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                 (aapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    bchainMove == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 aapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    bchainMove == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 aapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    bchainMove == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 aapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson True)) & 
                 (bapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    bapply_quas == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 bapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    bapply_quas == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 bapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    bapply_quas == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 bapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    bapply_quas == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) & 
              (fresh achainMove, bchainMove in
                 (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                    aapply_quas == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                    achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                 (aapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    bchainMove == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 aapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    bchainMove == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 aapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    bchainMove == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 aapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson True)) & 
                 (bapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    astep_state == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 bapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    astep_state == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 bapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    astep_state == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 bapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    astep_state == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) | 
              aeval == Move aapply_people & 
              (fresh achainMove in
                 (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                    bstep_state == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                    achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                 (aapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    bapply_quas == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 aapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    bapply_quas == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 aapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    bapply_quas == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 aapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    bapply_quas == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) & 
              (fresh achainMove in
                 (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                    aapply_quas == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                    achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                 (aapply_people == PA & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                    astep_state == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                 aapply_people == PB & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                    astep_state == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                 aapply_people == PC & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                    astep_state == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                 aapply_people == PD & 
                 (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                    achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                    astep_state == Qua amovePerson bmovePerson cmovePerson dmovePerson True))))) | 
        (fresh aisTorch, bisTorch, cisTorch, disTorch in
           bstep_state == Qua True aisTorch bisTorch cisTorch disTorch) & 
        (fresh anoTorch, bnoTorch, cnoTorch, dnoTorch in
           astep_state == Qua False anoTorch bnoTorch cnoTorch dnoTorch) & 
        (fresh astep_s'' in
           (fresh aapply_quas, bapply_quas in
              astep_s'' == State aapply_quas bapply_quas & 
              (fresh aapply_people, bapply_people in
                 aeval == Move aapply_people bapply_people & 
                 (fresh achainMove, bchainMove in
                    (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                       astep_state == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                       achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                    (aapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bchainMove == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    aapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bchainMove == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    aapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bchainMove == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    aapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson True)) & 
                    (bapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bapply_quas == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    bapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bapply_quas == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    bapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bapply_quas == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    bapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bapply_quas == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) & 
                 (fresh achainMove, bchainMove in
                    (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                       aapply_quas == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                       achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                    (aapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bchainMove == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    aapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bchainMove == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    aapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bchainMove == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    aapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson True)) & 
                    (bapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bstep_state == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    bapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bstep_state == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    bapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bstep_state == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    bapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       bchainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bstep_state == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) | 
                 aeval == Move aapply_people & 
                 (fresh achainMove in
                    (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                       astep_state == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                       achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                    (aapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bapply_quas == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    aapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bapply_quas == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    aapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bapply_quas == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    aapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bapply_quas == Qua amovePerson bmovePerson cmovePerson dmovePerson True))) & 
                 (fresh achainMove in
                    (fresh amoveTorch, bmoveTorch, cmoveTorch, dmoveTorch in
                       aapply_quas == Qua False amoveTorch bmoveTorch cmoveTorch dmoveTorch & 
                       achainMove == Qua True amoveTorch bmoveTorch cmoveTorch dmoveTorch) & 
                    (aapply_people == PA & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson False bmovePerson cmovePerson dmovePerson & 
                       bstep_state == Qua amovePerson True bmovePerson cmovePerson dmovePerson) | 
                    aapply_people == PB & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson False cmovePerson dmovePerson & 
                       bstep_state == Qua amovePerson bmovePerson True cmovePerson dmovePerson) | 
                    aapply_people == PC & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson False dmovePerson & 
                       bstep_state == Qua amovePerson bmovePerson cmovePerson True dmovePerson) | 
                    aapply_people == PD & 
                    (fresh amovePerson, bmovePerson, cmovePerson, dmovePerson in
                       achainMove == Qua amovePerson bmovePerson cmovePerson dmovePerson False & 
                       bstep_state == Qua amovePerson bmovePerson cmovePerson dmovePerson True))))) & 
           (fresh aswap, bswap in
              astep_s'' == State aswap bswap & 
              ceval == State bswap aswap)))) & 
     (Delay evalBridges ceval beval state'));

totalTime move outTime =
  move == Nil & 
  outTime == Zro | 
  (fresh atotalTime, btotalTime, ctotalTime, dtotalTime in
     move == Cons atotalTime ctotalTime & 
     (fresh apeople, bpeople, atime, btime in
        atotalTime == Move apeople bpeople & 
        (apeople == PA & 
        atime == Suc Zro | 
        apeople == PB & 
        atime == Suc (Suc Zro) | 
        apeople == PC & 
        atime == Suc (Suc (Suc (Suc (Suc Zro)))) | 
        apeople == PD & 
        atime == Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zro)))))))) & 
        (bpeople == PA & 
        btime == Suc Zro | 
        bpeople == PB & 
        btime == Suc (Suc Zro) | 
        bpeople == PC & 
        btime == Suc (Suc (Suc (Suc (Suc Zro)))) | 
        bpeople == PD & 
        btime == Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zro)))))))) & 
        (atime == Zro & 
        btotalTime == btime | 
        btime == Zro & 
        btotalTime == atime | 
        (fresh amaxo, bmaxo, cmaxo in
           atime == Suc amaxo & 
           btime == Suc bmaxo & 
           btotalTime == Suc cmaxo & 
           maxo amaxo bmaxo cmaxo)) | 
        atotalTime == Move apeople & 
        (apeople == PA & 
        btotalTime == Suc Zro | 
        apeople == PB & 
        btotalTime == Suc (Suc Zro) | 
        apeople == PC & 
        btotalTime == Suc (Suc (Suc (Suc (Suc Zro)))) | 
        apeople == PD & 
        btotalTime == Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zro))))))))) & 
     totalTime ctotalTime dtotalTime & 
     (btotalTime == Zro & 
     outTime == dtotalTime | 
     (fresh aadoo, badoo in
        btotalTime == Suc aadoo & 
        outTime == Suc badoo & 
        addo aadoo dtotalTime badoo)));

addo a b out =
  a == Zro & 
  out == b | 
  (fresh aadoo, badoo in
     a == Suc aadoo & 
     out == Suc badoo & 
     addo aadoo b badoo);

maxo a b out =
  a == Zro & 
  out == b | 
  b == Zro & 
  out == a | 
  (fresh amaxo, bmaxo, cmaxo in
     a == Suc amaxo & 
     b == Suc bmaxo & 
     out == Suc cmaxo & 
     maxo amaxo bmaxo cmaxo);

leo a b =
  a == Zro | 
  (fresh alteo, blteo in
     a == Suc alteo & 
     b == Suc blteo & 
     leo alteo blteo);

? fresh amainBridges in
    boundedEvalBridges (State (Qua True True True True True) (Qua False False False False False)) amainBridges (State (Qua False False False False False) (Qua True True True True True))