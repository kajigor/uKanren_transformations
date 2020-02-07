module Program.SpecialProp where 

import Syntax 
import Program.Bool
import Program.List

logintoQuery = Program loginto $ fresh ["s", "f", "r"] $ call "loginto" [V "s", V "f", trueo]

loginto :: [Def]
loginto = [logintoDef, lookupo, ando, oro, noto]
  where
    logintoDef =
      let subst = V "subst"
          formula = V "formula"
          result = V "result"
      in
      Def "loginto" ["subst", "formula", "result"] $
      fresh ["x", "y", "l", "r", "rl", "rr"] (
          (formula === true &&& result === trueo)
      ||| (formula === false &&& result === falso)
      ||| (formula === var (V "y") &&& call "lookupo" [subst, V "y", result])
      ||| (formula === neg (V "x")
           &&& call "loginto" [subst, V "x", V "rl"]
           &&& call "noto" [V "rl", result])
      ||| (formula === conj (V "l") (V "r")
           &&& call "loginto" [subst, V "l", V "rl"]
           &&& call "loginto" [subst, V "r", V "rr"]
           &&& call "ando" [V "rl", V "rr", result])
      ||| (formula === disj (V "l") (V "r")
           &&& call "loginto" [subst, V "l", V "rl"]
           &&& call "loginto" [subst, V "r", V "rr"]
           &&& call "oro" [V "rl", V "rr", result])
      )

    lookupo =
      let subst = V "subst"
          var   = V "var"
          result = V "result"
      in
      Def "lookupo" ["subst", "var", "result"] $ (
        fresh ["key", "val", "tail"] $
        (subst === pair (V "key") (V "val") % V "tail" &&& (
          (var === V "key" &&& result === V "val")
          ||| call "lookupo" [V "tail", var, result])
        )
      )

    oro =
      let result = V "result"
          a = V "a"
          b = V "b"
      in
      Def "oro" ["a", "b", "result"] $
        (a === falso &&& b === falso &&& result === falso) |||
        (a === falso &&& b === trueo &&& result === trueo) |||
        (a === trueo &&& b === falso &&& result === trueo) |||
        (a === trueo &&& b === trueo &&& result === trueo)

    ando =
      let result = V "result"
          a = V "a"
          b = V "b"
      in
      Def "ando" ["a", "b", "result"] $
        (a === falso &&& b === falso &&& result === falso) |||
        (a === falso &&& b === trueo &&& result === falso) |||
        (a === trueo &&& b === falso &&& result === falso) |||
        (a === trueo &&& b === trueo &&& result === trueo)

    noto =
      let result = V "result"
          a = V "a"
          b = V "b"
      in
      Def "noto" ["a", "result"] $
        (a === trueo &&& result === falso) |||
        (a === falso &&& result === trueo)

    pair x y = C "pair" [x, y]

    true = C "ltrue" []
    false = C "lfalse" []
    var x = C "var" [x]
    neg x = C "neg" [x]
    conj x y = C "conj" [x, y]
    disj x y = C "disj" [x, y]