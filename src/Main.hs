module Main where
import MuKanren
import Driver
import Debug.Trace
import Programs


run k a =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in take k $ eval a emptyState

main =
  do
    print $ drive (callFresh (\xs -> callFresh (\ys -> callFresh (\zs -> call (appendo xs ys zs) [xs, ys, zs]))))

    print $ drive (callFresh (\x ->
                    callFresh (\y ->
                    callFresh (\i ->
                    callFresh (\z ->
                    callFresh (\r ->
                    call (appendo x y i) [x, y, i] &&& call (appendo i z r) [i,z,r]))))))

    print $ drive (callFresh (\xs -> callFresh (\sx -> call (reverso xs sx) [xs, sx])))




--    print $ drive (call_fresh (\xs -> call_fresh (\ys -> (call_fresh (\zs -> call (appendo xs ys zs) [xs, ys, zs])))))

--    let l = list [at 239, at (-91)]


--smaller: ((((x.3 === []) &&& (x.4 === [])) ||| ((x.3 === (x.5, x.6)) &&& ((call reverso with [x.6; x.7; ]) &&& (call appendo with [x.7; (x.5, []); x.4; ])))) &&& (call appendo with [x.4; (x.2, []); x.1; ]))
--bigger: (((((x.6 === []) &&& (x.7 === [])) ||| ((x.6 === (x.8, x.9)) &&& ((call reverso with [x.9; x.10; ]) &&& (call appendo with [x.10; (x.8, []); x.7; ])))) &&& (call appendo with [x.7; (x.5, []); x.4; ])) &&& (call appendo with [x.4; (x.2, []); x.1; ]))
--    let bod = Uni Nil Nil
--    let smaller = ((((Var 3 === Nil) &&& (Var 4 === Nil)) ||| ((Var 3 === pair (Var 5) (Var 6)) &&& ((call (Fun "reverso" bod) [Var 6, Var 7 ]) &&& (call (Fun "appendo" bod) [Var 7 , pair (Var 5) Nil, Var 4])))) &&& (call (Fun "appendo" bod) [Var 4, pair (Var 2) Nil, Var 1 ]))
--    let bigger = (((((Var 6 === Nil) &&& (Var 7 === Nil)) ||| ((Var 6 === pair (Var 8) (Var 9)) &&& ((call (Fun "reverso" bod) [Var 9, Var 10]) &&& (call (Fun "appendo" bod) [Var 10, pair (Var 8) Nil, Var 7])))) &&& (call (Fun "appendo" bod) [Var 7, pair (Var 5) Nil, Var 4 ])) &&& (call (Fun "appendo" bod) [Var 4, pair (Var 2) Nil, Var 1 ]))
--    print $ embed smaller bigger

     -- print $ residualise (Leaf (Just ([(1::Var, Var 2),(0, Nil)],3)))

     -- print $ drive (callFresh (\xs -> callFresh (\ys -> callFresh  (\zs -> call (appendo xs ys zs) [xs, ys, zs]))))

     --print $ drive (callFresh (\xs -> callFresh (\sx -> call (reverso xs sx) [xs, sx])))
    {- let forward  l = run 1 (call_fresh (\xs -> call (reverso l xs) [l, xs]))
        backward l = run 2 (call_fresh (\xs -> call (reverso xs l) [xs, l]))
        empty = list []
        singleton = list [at 239]
        three = list [at 239, at 13, at (-91)]
        runtest t = do putStrLn $ "States stream: " ++ show t
                       print (reify' (Var 0) t)
    runtest (forward empty)
    runtest (forward singleton)
    runtest (forward three)
    runtest (forward empty)
    runtest (forward singleton)
    runtest (forward three)
    runtest (run 5 (call_fresh (\q -> call_fresh (\x -> (call_fresh (\y ->
      (q === pair x y) &&&
      call (appendo x x y) [x, x, y]))))))
    -}

    {-
    putStrLn "Forwards direction"
    -- An attempt to get more than one answer here leads to nontermination.
    -- The reason is that (appendo a' [239] b) produces infinite stream of states which
    -- are then bound to (reverso [] a') which refute every state except the one with a' === [],
    -- but to find that there is no "second" answer, we need to check every one of the states.
    let r' = run 1 (call_fresh (\xs -> call (reverso (list [at 239]) xs) [list [at 239], xs]))
    putStrLn $ "States stream: " ++ show r'
    putStrLn $ "Reified: " ++ show (reify' (Var 0) r')

    putStrLn "\nBackwards direction"
    -- Here, all the answers can be calculated.
    let r  = run 2 (call_fresh (\xs -> call (reverso xs (list [at 239])) [xs, list [at 239]]))
    putStrLn $ "States stream: " ++ show r
    putStrLn $ "Reified: " ++  show (reify' (Var 0) r)
    -}


