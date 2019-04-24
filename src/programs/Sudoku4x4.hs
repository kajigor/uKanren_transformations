module Sudoku4x4 where

import Syntax

query = fst sudoku4x4 $ fresh ["sudoku", "q"] (call "check_sudoku" [V "sudoku", V "q"])
queryValid   = fst sudoku4x4 $ fresh ["q"] (call "check_sudoku" [validField, V "q"])
queryInvalid = fst sudoku4x4 $ fresh ["q"] (call "check_sudoku" [invalidField, V "q"])
queryAllDiff = fst sudoku4x4 $ fresh ["q"] (call "all_different" [n1, n1, n1, n1, false])

n1 = C "n1" []
n2 = C "n2" []
n3 = C "n3" []
n4 = C "n4" []

false = C "false" []
true = C "true" []

-- (V "sudoku" === C "s4x4" [V "a11", V "a12", V "a13", V "a14", V "a21", V "a22", V "a23", V "a24", V "a31", V "a32", V "a33", V "a34", V "a41", V "a42", V "a43", V "a44"]) &&&

invalidField = C "s4x4" [ n1, n2, n3, n4
                        , n1, n2, n3, n4
                        , n1, n2, n3, n4
                        , n1, n2, n3, n4
                        ]

validField = C "s4x4" [ n2, n3, n1, n4
                      , n4, n1, n3, n2
                      , n3, n4, n2, n1
                      , n1, n2, n4, n3
                      ]

sudoku4x4 =
 (\last_goal ->
  Let (def "not_eq" ["a", "b", "q97"] (
    ((V "a" === C "n1" []) &&&
    (((V "b" === C "n1" []) &&&
    (V "q97" === C "false" [])) |||
    ((V "b" === C "n2" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n3" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n4" []) &&&
    (V "q97" === C "true" [])))) |||
    ((V "a" === C "n2" []) &&&
    (((V "b" === C "n1" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n2" []) &&&
    (V "q97" === C "false" [])) |||
    ((V "b" === C "n3" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n4" []) &&&
    (V "q97" === C "true" [])))) |||
    ((V "a" === C "n3" []) &&&
    (((V "b" === C "n1" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n2" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n3" []) &&&
    (V "q97" === C "false" [])) |||
    ((V "b" === C "n4" []) &&&
    (V "q97" === C "true" [])))) |||
    ((V "a" === C "n4" []) &&&
    (((V "b" === C "n1" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n2" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n3" []) &&&
    (V "q97" === C "true" [])) |||
    ((V "b" === C "n4" []) &&&
    (V "q97" === C "false" []))))
  )) (
  Let (def "all_different" ["a", "b", "c", "d", "q69"] (
    fresh ["q67", "q68"] (
      (call "not_eq" [V "a", V "b", V "q67"]) &&&
      (fresh ["q73", "q74"] (
         (call "not_eq" [V "a", V "c", V "q73"]) &&&
         (fresh ["q79", "q80"] (
            (call "not_eq" [V "a", V "d", V "q79"]) &&&
            (fresh ["q85", "q86"] (
               (call "not_eq" [V "b", V "c", V "q85"]) &&&
               (fresh ["q91", "q92"] (
                  (call "not_eq" [V "b", V "d", V "q91"]) &&&
                  (call "not_eq" [V "c", V "d", V "q92"]) &&&
                  (((V "q91" === C "false" []) &&&
                  (V "q86" === C "false" [])) |||
                  ((V "q91" === C "true" []) &&&
                  (V "q86" === V "q92"))))) &&&
               (((V "q85" === C "false" []) &&&
               (V "q80" === C "false" [])) |||
               ((V "q85" === C "true" []) &&&
               (V "q80" === V "q86"))))) &&&
            (((V "q79" === C "false" []) &&&
            (V "q74" === C "false" [])) |||
            ((V "q79" === C "true" []) &&&
            (V "q74" === V "q80"))))) &&&
         (((V "q73" === C "false" []) &&&
         (V "q68" === C "false" [])) |||
         ((V "q73" === C "true" []) &&&
         (V "q68" === V "q74"))))) &&&
      (((V "q67" === C "false" []) &&&
      (V "q69" === C "false" [])) |||
      ((V "q67" === C "true" []) &&&
      (V "q69" === V "q68"))))
  )) (
  Let (def "check_sudoku" ["sudoku", "q0"] (
    fresh ["a11", "a12", "a13", "a14", "a21", "a22", "a23", "a24", "a31", "a32", "a33", "a34", "a41", "a42", "a43", "a44"] (
      (V "sudoku" === C "s4x4" [V "a11", V "a12", V "a13", V "a14", V "a21", V "a22", V "a23", V "a24", V "a31", V "a32", V "a33", V "a34", V "a41", V "a42", V "a43", V "a44"]) &&&
      (fresh ["q1", "q2"] (
         (call "all_different" [V "a11", V "a12", V "a13", V "a14", V "q1"]) &&&
         (fresh ["q7", "q8"] (
            (call "all_different" [V "a21", V "a22", V "a23", V "a24", V "q7"]) &&&
            (fresh ["q13", "q14"] (
               (call "all_different" [V "a31", V "a32", V "a33", V "a34", V "q13"]) &&&
               (fresh ["q19", "q20"] (
                  (call "all_different" [V "a41", V "a42", V "a43", V "a44", V "q19"]) &&&
                  (fresh ["q25", "q26"] (
                     (call "all_different" [V "a11", V "a21", V "a31", V "a41", V "q25"]) &&&
                     (fresh ["q31", "q32"] (
                        (call "all_different" [V "a12", V "a22", V "a32", V "a42", V "q31"]) &&&
                        (fresh ["q37", "q38"] (
                           (call "all_different" [V "a13", V "a23", V "a33", V "a43", V "q37"]) &&&
                           (fresh ["q43", "q44"] (
                              (call "all_different" [V "a14", V "a24", V "a34", V "a44", V "q43"]) &&&
                              (fresh ["q49", "q50"] (
                                 (call "all_different" [V "a11", V "a12", V "a21", V "a22", V "q49"]) &&&
                                 (fresh ["q55", "q56"] (
                                    (call "all_different" [V "a13", V "a14", V "a23", V "a24", V "q55"]) &&&
                                    (fresh ["q61", "q62"] (
                                       (call "all_different" [V "a31", V "a32", V "a41", V "a42", V "q61"]) &&&
                                       (call "all_different" [V "a33", V "a34", V "a43", V "a44", V "q62"]) &&&
                                       (((V "q61" === C "false" []) &&&
                                       (V "q56" === C "false" [])) |||
                                       ((V "q61" === C "true" []) &&&
                                       (V "q56" === V "q62"))))) &&&
                                    (((V "q55" === C "false" []) &&&
                                    (V "q50" === C "false" [])) |||
                                    ((V "q55" === C "true" []) &&&
                                    (V "q50" === V "q56"))))) &&&
                                 (((V "q49" === C "false" []) &&&
                                 (V "q44" === C "false" [])) |||
                                 ((V "q49" === C "true" []) &&&
                                 (V "q44" === V "q50"))))) &&&
                              (((V "q43" === C "false" []) &&&
                              (V "q38" === C "false" [])) |||
                              ((V "q43" === C "true" []) &&&
                              (V "q38" === V "q44"))))) &&&
                           (((V "q37" === C "false" []) &&&
                           (V "q32" === C "false" [])) |||
                           ((V "q37" === C "true" []) &&&
                           (V "q32" === V "q38"))))) &&&
                        (((V "q31" === C "false" []) &&&
                        (V "q26" === C "false" [])) |||
                        ((V "q31" === C "true" []) &&&
                        (V "q26" === V "q32"))))) &&&
                     (((V "q25" === C "false" []) &&&
                     (V "q20" === C "false" [])) |||
                     ((V "q25" === C "true" []) &&&
                     (V "q20" === V "q26"))))) &&&
                  (((V "q19" === C "false" []) &&&
                  (V "q14" === C "false" [])) |||
                  ((V "q19" === C "true" []) &&&
                  (V "q14" === V "q20"))))) &&&
               (((V "q13" === C "false" []) &&&
               (V "q8" === C "false" [])) |||
               ((V "q13" === C "true" []) &&&
               (V "q8" === V "q14"))))) &&&
            (((V "q7" === C "false" []) &&&
            (V "q2" === C "false" [])) |||
            ((V "q7" === C "true" []) &&&
            (V "q2" === V "q8"))))) &&&
         (((V "q1" === C "false" []) &&&
         (V "q0" === C "false" [])) |||
         ((V "q1" === C "true" []) &&&
         (V "q0" === V "q2"))))))
  )) (
  last_goal)))

  ,

  "open MiniKanren\nopen MiniKanrenStd\ntype num =\n  | N1 \n  | N2 \n  | N3 \n  | N4 \nlet n1 () = !! N1\nlet n2 () = !! N2\nlet n3 () = !! N3\nlet n4 () = !! N4\ntype 'a0 gsudoku4X4 =\n  | S4x4 of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 * 'a0 *\n  'a0 * 'a0 * 'a0 * 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | S4x4\n      (a0_0, a0_1, a0_2, a0_3, a0_4, a0_5, a0_6, a0_7, a0_8, a0_9, a0_10,\n       a0_11, a0_12, a0_13, a0_14, a0_15)\n      ->\n      S4x4\n        ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3), (fa0 a0_4),\n          (fa0 a0_5), (fa0 a0_6), (fa0 a0_7), (fa0 a0_8), (fa0 a0_9),\n          (fa0 a0_10), (fa0 a0_11), (fa0 a0_12), (fa0 a0_13), (fa0 a0_14),\n          (fa0 a0_15))\nmodule For_gsudoku4X4 =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | S4x4\n                 (a0_0, a0_1, a0_2, a0_3, a0_4, a0_5, a0_6, a0_7, a0_8, a0_9,\n                  a0_10, a0_11, a0_12, a0_13, a0_14, a0_15)\n                 ->\n                 S4x4\n                   ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3),\n                     (fa0 a0_4), (fa0 a0_5), (fa0 a0_6), (fa0 a0_7),\n                     (fa0 a0_8), (fa0 a0_9), (fa0 a0_10), (fa0 a0_11),\n                     (fa0 a0_12), (fa0 a0_13), (fa0 a0_14), (fa0 a0_15))\n           type 'a0 t = 'a0 gsudoku4X4\n         end)\nlet rec s4x4 x__0 x__1 x__2 x__3 x__4 x__5 x__6 x__7 x__8 x__9 x__10 x__11\n  x__12 x__13 x__14 x__15 =\n  inj\n    (For_gsudoku4X4.distrib\n       (S4x4\n          (x__0, x__1, x__2, x__3, x__4, x__5, x__6, x__7, x__8, x__9, x__10,\n            x__11, x__12, x__13, x__14, x__15)))")
