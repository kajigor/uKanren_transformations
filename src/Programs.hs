module Programs where
import MuKanren

appendo a b ab =
  fun "appendo" $
      conde [ [ a === nil, b === ab ]
            , [ callFresh (\h ->
                  callFresh (\t ->
                    a === pair h t &&&
                    callFresh (\ab' ->
                      pair h ab' === ab &&& (call (appendo t b ab') [t, b, ab']))
                 )
                )
              ]
            ]

reverso a b =
  fun "reverso" $
      conde [ [a === nil, b === nil]
            , [callFresh (\h ->
                callFresh (\t ->
                    a === pair h t &&&
                    callFresh (\a' ->
                      (call (reverso t a') [t, a'])
                       &&&
                       (let h' = list [h] in (call (appendo a' h' b) [a', h', b]))
                  )
                )
              )]
            ]

