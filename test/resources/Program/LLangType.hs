module Program.LLangType where

import Syntax
import Program.Bool
import Program.List
import Program.Option
import Program.Pair

query  = Program typecheck $ fresh ["t", "gamma"] (call "typecheck_" [V "gamma", V "t", some integer])
query1 = Program typecheck $ fresh ["t"] (call "typecheck_" [nil, V "t", some integer])


integer :: Term a
integer = C "integer" []

boolean :: Term a
boolean = C "boolean" []

typeEqDef :: Def
typeEqDef =
    ( Def "typeEq" ["x", "y", "q86"]
      ( fresh ["q87"]
        (
          ( q87 === pair x y ) &&&
          ( ( q87 === pair integer integer &&& q86 === trueo ) |||
            ( q87 === pair boolean boolean &&& q86 === trueo ) |||
            ( q87 === pair integer boolean &&& q86 === falso ) |||
            ( q87 === pair boolean integer &&& q86 === falso )
          )
        )
      )
    )
  where
    [x, y, q86, q87] = map V ["x", "y", "q86", "q87"]

typeEq :: [Def]
typeEq = [typeEqDef]

typecheckDef :: Def
typecheckDef =
    ( Def "typecheck_" ["gamma", "term", "q0"]
      (
        ( fresh ["q1"] (term === C "iConst_" [q1] &&& q0 === some integer ) ) |||
        ( fresh ["q3"] (term === C "bConst_" [q3] &&& q0 === some boolean ) ) |||
        ( fresh ["v"] (term === C "var_" [v] &&& call "nthOpt" [gamma, v, q0] ) ) |||
        ( fresh ["x", "y", "q6"]
          ( term === C "plus_" [x, y] &&&
            call "typecheck_" [gamma, x, q6] &&&
            ( ( q6 === none &&& q0 === none ) |||
              ( fresh ["x'", "q9"]
                ( q6 === some x' &&& call "typecheck_" [gamma, y, q9] &&&
                  ( ( q9 === none &&& q0 === none ) |||
                    ( fresh ["y'", "q12", "q15", "q16"]
                      ( q9 === some y' &&&
                        call "typeEq" [x', integer, q15] &&&
                        call "typeEq" [y', integer, q16] &&&
                        ( ( q15 === falso &&& q12 === falso ) |||
                          ( q15 === trueo &&& q12 === q16 )
                        ) &&&
                        ( ( q12 === trueo &&& q0 === some integer ) |||
                          ( q12 === falso &&& q0 === none )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ) |||
        (fresh ["x", "y", "q22"]
          ( term === C "mult_" [x, y] &&&
            call "typecheck_" [gamma, x, q22] &&&
            ( ( q22 === none &&& q0 === none ) |||
              ( fresh ["x'", "q25"]
                ( q22 === some x' &&&
                  call "typecheck_" [gamma, y, q25] &&&
                  ( ( q25 === none &&& q0 === none ) |||
                    ( fresh ["y'", "q28", "q31", "q32"]
                      ( q25 === some y' &&&
                        call "typeEq" [x', integer, q31] &&&
                        call "typeEq" [y', integer, q32] &&&
                        ( ( q31 === falso &&& q28 === falso ) |||
                          ( q31 === trueo &&& q28 === q32 )
                        ) &&&
                        ( (q28 === trueo &&& q0 === some integer ) |||
                          (q28 === falso &&& q0 === none )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ) |||
        ( fresh ["x", "y", "q38"]
          ( term === C "equal_" [x, y] &&&
            call "typecheck_" [gamma, x, q38] &&&
            ( ( q38 === none &&& q0 === none ) |||
              ( fresh ["x'", "q41"]
                ( q38 === some x' &&&
                  call "typecheck_" [gamma, y, q41] &&&
                  ( ( q41 === none &&& q0 === none ) |||
                    ( fresh ["y'", "q44"]
                      ( q41 === some y' &&&
                        call "typeEq" [x', y', q44] &&&
                        ( (q44 === trueo &&& q0 === some boolean ) |||
                          (q44 === falso &&& q0 === none )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ) |||
        ( fresh ["x", "y", "q48"]
          ( term === C "less_" [x, y] &&&
            call "typecheck_" [gamma, x, q48] &&&
            ( ( q48 === none &&& q0 === none ) |||
              ( fresh ["x'", "q51"]
                ( q48 === some x' &&&
                  call "typecheck_" [gamma, y, q51] &&&
                  ( ( q51 === none &&& q0 === none ) |||
                    ( fresh ["y'", "q54", "q57", "q58"]
                      ( ( q51 === some y' &&&
                          call "typeEq" [x', integer, q57] &&&
                          call "typeEq" [y', integer, q58] &&&
                          ( ( q57 === falso &&& q54 === falso ) |||
                            ( q57 === trueo &&& q54 === q58 )
                          ) &&&
                          ( (q54 === trueo &&& q0 === some boolean ) |||
                            (q54 === falso &&& q0 === none )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ) |||
        ( fresh ["c", "t", "e", "q64"]
          ( term === C "if_" [c, t, e] &&&
            call "typecheck_" [gamma, c, q64] &&&
            ( ( q64 === none &&& q0 === none ) |||
              ( fresh ["c'", "q67"]
                ( q64 === some c' &&&
                  call "typeEq" [c', boolean, q67] &&&
                  ( ( fresh ["q69"]
                      ( q67 === trueo &&&
                        call "typecheck_" [gamma, t, q69] &&&
                        ( ( q69 === none &&& q0 === none ) |||
                          ( fresh ["t'", "q72"]
                            ( q69 === some t' &&&
                              call "typecheck_" [gamma, e, q72] &&&
                              ( ( q72 === none &&& q0 === none ) |||
                                ( fresh ["e'", "q75"]
                                  ( q72 === some e' &&&
                                    call "typeEq" [t', e', q75] &&&
                                    ( ( q75 === trueo &&& q0 === some t') |||
                                      ( q75 === falso &&& q0 === none)
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ) |||
                    ( q67 === falso &&& q0 === none)
                  )
                )
              )
            )
          )
        ) |||
        ( fresh ["v", "b", "q80"]
          ( term === C "let_" [v, b] &&&
            call "typecheck_" [gamma, v, q80] &&&
            ( ( q80 === none &&& q0 === none ) |||
              ( fresh ["v'"]
                ( q80 === some v' &&&
                  call "typecheck_" [v' % gamma, b, q0]
                )
              )
            )
          )
        )
      )
    )
  where
    [gamma, term, q0, q1, q3, v, x, y, q6, q9, x', y',
     q12, q15, q16, q22, q25, q28, q31, q32, q38, q41,
     q44, q48, q51, q54, q57, q58, c, t, e, c', t', e',
     q64, q67, q69, q72, q75, b, q80, v'] =
       map V
           ["gamma", "term", "q0", "q1", "q3", "v", "x", "y", "q6", "q9", "x'", "y'",
            "q12", "q15", "q16", "q22", "q25", "q28", "q31", "q32", "q38", "q41",
            "q44", "q48", "q51", "q54", "q57", "q58", "c", "t", "e", "c'", "t'", "e'",
            "q64", "q67", "q69", "q72", "q75", "b", "q80", "v'"]

typecheck :: [Def]
typecheck = typecheckDef : nthOpt ++ typeEq