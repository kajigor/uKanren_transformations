:: ando x y b =
  (x === trueo /\ y === trueo /\ b === trueo) \/
  (x === falso /\ y === trueo /\ b === falso) \/
  (x === trueo /\ y === falso /\ b === falso) \/
  (x === falso /\ y === falso /\ b === falso)


:: oro x y b =
  (x === trueo /\ y === trueo /\ b === trueo) \/
  (x === falso /\ y === trueo /\ b === trueo) \/
  (x === trueo /\ y === falso /\ b === trueo) \/
  (x === falso /\ y === falso /\ b === falso)

:: noto x b =
  (x === trueo /\ b === falso) \/
  (x === falso /\ b === trueo)

:: evalo st fm u =
  [ x y v w :
    ({evalo st x v} /\ (
      (fm === <neg: x> /\ {noto v u}) \/
      ({evalo st y w}/\
        ((fm === <conj: x y> /\ {ando v w u}) \/
         (fm === <disj: x y> /\ {oro v w u})))))] \/
  [ z: (fm === <var: z> /\ {elemo z st u})]

:: elemo n s v =
  [h t n':
    (n === zero /\ s === h % t /\ v === h) \/
    (n === succ n' /\ s === h % t /\ {elemo n' t v})
  ]

? {evalo st fm trueo}
