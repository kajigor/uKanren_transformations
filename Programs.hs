module Programs where 
import MuKanren

appendo xs ys zs = 
  fun "appendo" $
      conde [ [xs === nil, zs === ys]
            , [call_fresh 
                (\h -> call_fresh 
                  (\t -> 
                    (xs === pair h t) 
                    &&& (call_fresh (\r -> (zs === pair h r) 
                                       &&& (call (appendo t ys r) [t, ys, r])))
                  )
                )
              ]
            ]

reverso xs ys = 
  fun "reverso" $
      conde [ [xs === nil, ys === nil]
            , [call_fresh (\h ->
                call_fresh (\t -> 
                  call_fresh (\xs' -> 
                    xs === pair h t
                    &&& call (reverso t xs') [t, xs']
                    &&& (let h' = list [h] in call (appendo xs' h' ys) [xs', h', ys])
                  )
                )
              )]
            ]

{-
let rec reverso a b =
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; Fresh.two (fun h t ->
          (a === h%t) &&&
          (Fresh.one (fun a' ->
              (appendo a' !<h b) &&& (reverso t a')
          ))
      )
    ]
-}
