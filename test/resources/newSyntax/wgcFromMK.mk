evalWGC state moves state' =
  moves == Nil & 
  state == state' | 
  (fresh a, b, c in
     moves == Cons a moves & 
     (fresh as, bs in
        state == State as bs & 
        ((fresh a, b, c in
            as == Qua a b c True) & 
        (fresh a, b, c in
           bs == Qua a b c False) & 
        (fresh al, bl, cl, dl, ar, br, cr, dr in
           as == Qua al bl cl dl & 
           bs == Qua ar br cr dr & 
           (a == Empty & 
           c == State (Qua al bl cl False) (Qua ar br cr True) & 
           (fresh a, b in
              c == State a b & 
              ((fresh a, b, c in
                  a == Qua a b c True) | 
              (fresh a, b, c in
                 a == Qua a b c False) & 
              ((fresh a, b, c in
                  a == Qua False a b c) | 
              (fresh a, b, c in
                 a == Qua True a b c) & 
              ((fresh a, b, c in
                  a == Qua a b False c) & 
              (fresh a, b, c in
                 a == Qua a False b c) | 
              (fresh a, b, c in
                 a == Qua a b True c) & 
              (fresh a, b, c in
                 a == Qua a True b c)))) & 
              ((fresh a, b, c in
                  b == Qua a b c True) | 
              (fresh a, b, c in
                 b == Qua a b c False) & 
              ((fresh a, b, c in
                  b == Qua False a b c) | 
              (fresh a, b, c in
                 b == Qua True a b c) & 
              ((fresh a, b, c in
                  b == Qua a b False c) & 
              (fresh a, b, c in
                 b == Qua a False b c) | 
              (fresh a, b, c in
                 b == Qua a b True c) & 
              (fresh a, b, c in
                 b == Qua a True b c))))) | 
           a == Goat & 
           (fresh a, b, c in
              as == Qua True a b c) & 
           c == State (Qua False bl cl False) (Qua True br cr True) & 
           (fresh a, b in
              c == State a b & 
              ((fresh a, b, c in
                  a == Qua a b c True) | 
              (fresh a, b, c in
                 a == Qua a b c False) & 
              ((fresh a, b, c in
                  a == Qua False a b c) | 
              (fresh a, b, c in
                 a == Qua True a b c) & 
              ((fresh a, b, c in
                  a == Qua a b False c) & 
              (fresh a, b, c in
                 a == Qua a False b c) | 
              (fresh a, b, c in
                 a == Qua a b True c) & 
              (fresh a, b, c in
                 a == Qua a True b c)))) & 
              ((fresh a, b, c in
                  b == Qua a b c True) | 
              (fresh a, b, c in
                 b == Qua a b c False) & 
              ((fresh a, b, c in
                  b == Qua False a b c) | 
              (fresh a, b, c in
                 b == Qua True a b c) & 
              ((fresh a, b, c in
                  b == Qua a b False c) & 
              (fresh a, b, c in
                 b == Qua a False b c) | 
              (fresh a, b, c in
                 b == Qua a b True c) & 
              (fresh a, b, c in
                 b == Qua a True b c))))) | 
           a == Wolf & 
           (fresh a, b, c in
              as == Qua a True b c) & 
           c == State (Qua al False cl False) (Qua ar True cr True) & 
           (fresh a, b in
              c == State a b & 
              ((fresh a, b, c in
                  a == Qua a b c True) | 
              (fresh a, b, c in
                 a == Qua a b c False) & 
              ((fresh a, b, c in
                  a == Qua False a b c) | 
              (fresh a, b, c in
                 a == Qua True a b c) & 
              ((fresh a, b, c in
                  a == Qua a b False c) & 
              (fresh a, b, c in
                 a == Qua a False b c) | 
              (fresh a, b, c in
                 a == Qua a b True c) & 
              (fresh a, b, c in
                 a == Qua a True b c)))) & 
              ((fresh a, b, c in
                  b == Qua a b c True) | 
              (fresh a, b, c in
                 b == Qua a b c False) & 
              ((fresh a, b, c in
                  b == Qua False a b c) | 
              (fresh a, b, c in
                 b == Qua True a b c) & 
              ((fresh a, b, c in
                  b == Qua a b False c) & 
              (fresh a, b, c in
                 b == Qua a False b c) | 
              (fresh a, b, c in
                 b == Qua a b True c) & 
              (fresh a, b, c in
                 b == Qua a True b c))))) | 
           a == Cabbage & 
           (fresh a, b, c in
              as == Qua a b True c) & 
           c == State (Qua al bl False False) (Qua ar br True True) & 
           (fresh a, b in
              c == State a b & 
              ((fresh a, b, c in
                  a == Qua a b c True) | 
              (fresh a, b, c in
                 a == Qua a b c False) & 
              ((fresh a, b, c in
                  a == Qua False a b c) | 
              (fresh a, b, c in
                 a == Qua True a b c) & 
              ((fresh a, b, c in
                  a == Qua a b False c) & 
              (fresh a, b, c in
                 a == Qua a False b c) | 
              (fresh a, b, c in
                 a == Qua a b True c) & 
              (fresh a, b, c in
                 a == Qua a True b c)))) & 
              ((fresh a, b, c in
                  b == Qua a b c True) | 
              (fresh a, b, c in
                 b == Qua a b c False) & 
              ((fresh a, b, c in
                  b == Qua False a b c) | 
              (fresh a, b, c in
                 b == Qua True a b c) & 
              ((fresh a, b, c in
                  b == Qua a b False c) & 
              (fresh a, b, c in
                 b == Qua a False b c) | 
              (fresh a, b, c in
                 b == Qua a b True c) & 
              (fresh a, b, c in
                 b == Qua a True b c))))))) | 
        (fresh a, b, c in
           bs == Qua a b c True) & 
        (fresh a, b, c in
           as == Qua a b c False) & 
        (fresh as'' in
           (fresh al, bl, cl, dl, ar, br, cr, dr in
              bs == Qua al bl cl dl & 
              as == Qua ar br cr dr & 
              (a == Empty & 
              as'' == State (Qua al bl cl False) (Qua ar br cr True) & 
              (fresh a, b in
                 as'' == State a b & 
                 ((fresh a, b, c in
                     a == Qua a b c True) | 
                 (fresh a, b, c in
                    a == Qua a b c False) & 
                 ((fresh a, b, c in
                     a == Qua False a b c) | 
                 (fresh a, b, c in
                    a == Qua True a b c) & 
                 ((fresh a, b, c in
                     a == Qua a b False c) & 
                 (fresh a, b, c in
                    a == Qua a False b c) | 
                 (fresh a, b, c in
                    a == Qua a b True c) & 
                 (fresh a, b, c in
                    a == Qua a True b c)))) & 
                 ((fresh a, b, c in
                     b == Qua a b c True) | 
                 (fresh a, b, c in
                    b == Qua a b c False) & 
                 ((fresh a, b, c in
                     b == Qua False a b c) | 
                 (fresh a, b, c in
                    b == Qua True a b c) & 
                 ((fresh a, b, c in
                     b == Qua a b False c) & 
                 (fresh a, b, c in
                    b == Qua a False b c) | 
                 (fresh a, b, c in
                    b == Qua a b True c) & 
                 (fresh a, b, c in
                    b == Qua a True b c))))) | 
              a == Goat & 
              (fresh a, b, c in
                 bs == Qua True a b c) & 
              as'' == State (Qua False bl cl False) (Qua True br cr True) & 
              (fresh a, b in
                 as'' == State a b & 
                 ((fresh a, b, c in
                     a == Qua a b c True) | 
                 (fresh a, b, c in
                    a == Qua a b c False) & 
                 ((fresh a, b, c in
                     a == Qua False a b c) | 
                 (fresh a, b, c in
                    a == Qua True a b c) & 
                 ((fresh a, b, c in
                     a == Qua a b False c) & 
                 (fresh a, b, c in
                    a == Qua a False b c) | 
                 (fresh a, b, c in
                    a == Qua a b True c) & 
                 (fresh a, b, c in
                    a == Qua a True b c)))) & 
                 ((fresh a, b, c in
                     b == Qua a b c True) | 
                 (fresh a, b, c in
                    b == Qua a b c False) & 
                 ((fresh a, b, c in
                     b == Qua False a b c) | 
                 (fresh a, b, c in
                    b == Qua True a b c) & 
                 ((fresh a, b, c in
                     b == Qua a b False c) & 
                 (fresh a, b, c in
                    b == Qua a False b c) | 
                 (fresh a, b, c in
                    b == Qua a b True c) & 
                 (fresh a, b, c in
                    b == Qua a True b c))))) | 
              a == Wolf & 
              (fresh a, b, c in
                 bs == Qua a True b c) & 
              as'' == State (Qua al False cl False) (Qua ar True cr True) & 
              (fresh a, b in
                 as'' == State a b & 
                 ((fresh a, b, c in
                     a == Qua a b c True) | 
                 (fresh a, b, c in
                    a == Qua a b c False) & 
                 ((fresh a, b, c in
                     a == Qua False a b c) | 
                 (fresh a, b, c in
                    a == Qua True a b c) & 
                 ((fresh a, b, c in
                     a == Qua a b False c) & 
                 (fresh a, b, c in
                    a == Qua a False b c) | 
                 (fresh a, b, c in
                    a == Qua a b True c) & 
                 (fresh a, b, c in
                    a == Qua a True b c)))) & 
                 ((fresh a, b, c in
                     b == Qua a b c True) | 
                 (fresh a, b, c in
                    b == Qua a b c False) & 
                 ((fresh a, b, c in
                     b == Qua False a b c) | 
                 (fresh a, b, c in
                    b == Qua True a b c) & 
                 ((fresh a, b, c in
                     b == Qua a b False c) & 
                 (fresh a, b, c in
                    b == Qua a False b c) | 
                 (fresh a, b, c in
                    b == Qua a b True c) & 
                 (fresh a, b, c in
                    b == Qua a True b c))))) | 
              a == Cabbage & 
              (fresh a, b, c in
                 bs == Qua a b True c) & 
              as'' == State (Qua al bl False False) (Qua ar br True True) & 
              (fresh a, b in
                 as'' == State a b & 
                 ((fresh a, b, c in
                     a == Qua a b c True) | 
                 (fresh a, b, c in
                    a == Qua a b c False) & 
                 ((fresh a, b, c in
                     a == Qua False a b c) | 
                 (fresh a, b, c in
                    a == Qua True a b c) & 
                 ((fresh a, b, c in
                     a == Qua a b False c) & 
                 (fresh a, b, c in
                    a == Qua a False b c) | 
                 (fresh a, b, c in
                    a == Qua a b True c) & 
                 (fresh a, b, c in
                    a == Qua a True b c)))) & 
                 ((fresh a, b, c in
                     b == Qua a b c True) | 
                 (fresh a, b, c in
                    b == Qua a b c False) & 
                 ((fresh a, b, c in
                     b == Qua False a b c) | 
                 (fresh a, b, c in
                    b == Qua True a b c) & 
                 ((fresh a, b, c in
                     b == Qua a b False c) & 
                 (fresh a, b, c in
                    b == Qua a False b c) | 
                 (fresh a, b, c in
                    b == Qua a b True c) & 
                 (fresh a, b, c in
                    b == Qua a True b c))))))) & 
           (fresh a, b in
              as'' == State a b & 
              c == State b a)))) & 
     evalWGC c b state');

? fresh a in
    evalWGC (State (Qua True True True True) (Qua False False False False)) a (State (Qua False False False False) (Qua True True True True))