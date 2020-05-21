open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec evalo y0 y1 =
    fresh (q1 q2 q3 q4)
      ( y1 === conj q1 q2 &&& _evaloEvaloNando y0 q2 q3 q1 q3
      ||| (y1 === disj q1 q2 &&& evaloEvaloNandoNandoNando y0 q1 q2)
      ||| (y1 === neg q1 &&& _evalo y0 q1 !!false)
      ||| (y1 === var q4 &&& elemo !!true q4 y0) )
  and _evaloEvaloNando y7 y8 y9 y11 y12 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y11 === conj q1 q2
      &&& (evaloNandoNando y12 y7 q2 q3 &&& _evaloEvaloNando y7 y8 y9 q1 q3)
      ||| (y11 === disj q1 q2 &&& (_evaloEvaloNandoNando y7 q1 q2 q4 q5 &&& _evaloNando y7 y8 y9 q6 !!false &&& _nando q4 q5 y12))
      ||| (y11 === neg q1 &&& (_evaloEvaloNando y7 y8 y9 q1 q3 &&& _nando q3 q3 y12))
      ||| (y11 === var q7 &&& elemoEvaloNando y7 y8 y9 y12 q7) )
  and elemoEvaloNando y13 y14 y15 y17 y18 =
    fresh (q1 q2 q3 q4)
      ( y18 === Nat.zero
      &&& (y13 === ( % ) y17 q1)
      &&& _evaloNando (( % ) y17 q1) y14 y15 q2 !!false
      ||| (y18 === Nat.succ q3 &&& (y13 === ( % ) q4 q1) &&& (_evaloNando (( % ) q4 q1) y14 y15 q2 !!false &&& elemo y17 q3 q1)) )
  and elemo y19 y20 y21 =
    fresh (q1 q2 q3) (y20 === Nat.zero &&& (y21 === ( % ) y19 q1) ||| (y20 === Nat.succ q2 &&& (y21 === ( % ) q3 q1) &&& elemo y19 q2 q1))
  and evaloNandoNando y28 y29 y30 y31 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y30 === conj q1 q2
      &&& (evaloNandoNando q3 y29 q2 q4 &&& _nandoNando q5 q5 y28 y31 q3 &&& _evalo y29 q1 q4)
      ||| (y30 === disj q1 q2 &&& (_evaloEvaloNandoNando y29 q1 q2 q6 q7 &&& nandoNandoNando y28 y31 q6 q7))
      ||| (y30 === neg q1 &&& evaloNandoNandoNando y29 q5 q5 y28 q1 y31 q3)
      ||| (y30 === var q8 &&& elemoNandoNando y28 y29 y31 q8) )
  and nandoNandoNando y34 y35 y38 y39 =
    fresh (q1)
      ( y38 === !!false &&& (y39 === !!false) &&& _nandoNando q1 q1 y34 y35 !!true
      ||| (y38 === !!false &&& (y39 === !!true) &&& _nandoNando q1 q1 y34 y35 !!true)
      ||| (y38 === !!true &&& (y39 === !!false) &&& _nandoNando q1 q1 y34 y35 !!true)
      ||| (y38 === !!true &&& (y39 === !!true) &&& _nandoNando q1 q1 y34 y35 !!false) )
  and _nando y40 y41 y42 =
    y40 === !!false &&& (y41 === !!false) &&& (y42 === !!true)
    ||| (y40 === !!false &&& (y41 === !!true) &&& (y42 === !!true))
    ||| (y40 === !!true &&& (y41 === !!false) &&& (y42 === !!true))
    ||| (y40 === !!true &&& (y41 === !!true) &&& (y42 === !!false))
  and elemoNandoNando y43 y44 y45 y48 =
    fresh (q1 q2 q3 q4 q5)
      ( y48 === Nat.zero
      &&& (y44 === ( % ) q1 q2)
      &&& _nandoNando q3 q3 y43 y45 q1
      ||| (y48 === Nat.succ q4 &&& (y44 === ( % ) q5 q2) &&& elemoNandoNando y43 q2 y45 q4) )
  and _evaloNandoNando y49 y50 y51 y53 y54 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y51 === conj q1 q2
      &&& (evaloNandoNando q3 y50 q2 q4 &&& _nandoNando y53 y54 y49 q3 q3 &&& _evalo y50 q1 q4)
      ||| (y51 === disj q1 q2 &&& (_evaloEvaloNandoNando y50 q1 q2 q5 q6 &&& __nandoNandoNando y49 y53 y54 q5 q6))
      ||| (y51 === neg q1 &&& evaloNandoNandoNando y50 y53 y54 y49 q1 q3 q3)
      ||| (y51 === var q7 &&& _elemoNandoNando y49 y50 y53 y54 q7) )
  and _evaloNando y59 y60 y61 y62 y63 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y60 === conj q1 q2
      &&& (evaloNandoNando y62 y59 q2 q3 &&& _evalo y59 q1 q3 &&& _nando y61 y62 y63)
      ||| (y60 === disj q1 q2 &&& (_evaloEvaloNandoNando y59 q1 q2 q4 q5 &&& _nandoNando y61 y62 y63 q4 q5))
      ||| (y60 === neg q1 &&& _evaloNandoNando y63 y59 q1 y61 y62)
      ||| (y60 === var q6 &&& elemoNando y59 y61 y62 y63 q6) )
  and evaloNandoNandoNando y64 y65 y66 y67 y68 y70 y71 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y68 === conj q1 q2
      &&& (evaloNandoNando q3 y64 q2 q4 &&& _nandoNandoNando y65 y66 y67 q3 y70 y71 &&& _evalo y64 q1 q4)
      ||| (y68 === disj q1 q2 &&& (_evaloEvaloNandoNando y64 q1 q2 q5 q6 &&& __nandoNandoNando y66 y70 y71 q5 q6 &&& _nando y65 y66 y67))
      ||| (y68 === neg q1 &&& (evaloNandoNandoNando y64 y70 y71 y66 q1 q3 q3 &&& _nando y65 y66 y67))
      ||| (y68 === var q7 &&& elemoNandoNandoNando y64 y65 y66 y67 y70 y71 q7) )
  and _nandoNandoNando y72 y73 y74 y75 y76 y77 =
    y75 === !!false &&& (y77 === !!true) &&& _nandoNando y72 y73 y74 y76 !!true
    ||| (y75 === !!true &&& (y77 === !!false) &&& _nandoNando y72 y73 y74 y76 !!false)
  and _nandoNando y78 y79 y80 y81 y82 =
    y81 === !!false &&& (y82 === !!false) &&& (y79 === !!true) &&& _nando y78 !!true y80
    ||| (y81 === !!false &&& (y82 === !!true) &&& (y79 === !!true) &&& _nando y78 !!true y80)
    ||| (y81 === !!true &&& (y82 === !!false) &&& (y79 === !!true) &&& _nando y78 !!true y80)
    ||| (y81 === !!true &&& (y82 === !!true) &&& (y79 === !!false) &&& _nando y78 !!false y80)
  and elemoNandoNandoNando y83 y84 y85 y86 y88 y89 y90 =
    fresh (q1 q2 q3 q4)
      ( y90 === Nat.zero
      &&& (y83 === ( % ) q1 q2)
      &&& _nandoNandoNando y84 y85 y86 q1 y88 y89
      ||| (y90 === Nat.succ q3 &&& (y83 === ( % ) q4 q2) &&& elemoNandoNandoNando q2 y84 y85 y86 y88 y89 q3) )
  and elemoNando y91 y92 y93 y94 y95 =
    fresh (q1 q2 q3)
      ( y95 === Nat.zero
      &&& (y91 === ( % ) y93 q1)
      &&& _nando y92 y93 y94
      ||| (y95 === Nat.succ q2 &&& (y91 === ( % ) q3 q1) &&& elemoNando q1 y92 y93 y94 q2) )
  and __nandoNandoNando y96 y98 y99 y100 y101 =
    y100 === !!false &&& (y101 === !!false) &&& _nandoNando y98 y99 y96 !!true !!true
    ||| (y100 === !!false &&& (y101 === !!true) &&& _nandoNando y98 y99 y96 !!true !!true)
    ||| (y100 === !!true &&& (y101 === !!false) &&& _nandoNando y98 y99 y96 !!true !!true)
    ||| (y100 === !!true &&& (y101 === !!true) &&& _nandoNando y98 y99 y96 !!false !!false)
  and _elemoNandoNando y102 y103 y105 y106 y107 =
    fresh (q1 q2 q3 q4)
      ( y107 === Nat.zero
      &&& (y103 === ( % ) q1 q2)
      &&& _nandoNando y105 y106 y102 q1 q1
      ||| (y107 === Nat.succ q3 &&& (y103 === ( % ) q4 q2) &&& _elemoNandoNando y102 q2 y105 y106 q3) )
  and _evalo y108 y109 y110 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y109 === conj q1 q2
      &&& (evaloNandoNando y110 y108 q2 q3 &&& _evalo y108 q1 q3)
      ||| (y109 === disj q1 q2 &&& (_evaloEvaloNandoNando y108 q1 q2 q4 q5 &&& _nando q4 q5 y110))
      ||| (y109 === neg q1 &&& _evaloNando y108 q1 q3 q3 y110)
      ||| (y109 === var q6 &&& elemo y110 q6 y108) )
  and __nandoNando y111 y112 y113 y114 =
    y112 === !!false &&& (y113 === !!true) &&& _nando !!true y114 y111 ||| (y112 === !!true &&& (y113 === !!false) &&& _nando !!false y114 y111)
  and evaloEvaloNandoNandoNando y115 y116 y117 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y116 === conj q1 q2
      &&& (evaloEvaloNandoNando y115 y117 q3 q1 q4 &&& evaloNandoNando q5 y115 q2 q4 &&& _nando q5 q5 q3)
      ||| (y116 === disj q1 q2 &&& (_evaloEvaloNandoNando y115 q1 q2 q6 q7 &&& _evaloNandoNando !!true y115 y117 q3 q8 &&& _nandoNando q5 q5 q3 q6 q7))
      ||| (y116 === neg q1 &&& (evaloEvaloNandoNando y115 y117 q3 q1 q4 &&& __nandoNando q3 q4 q5 q5))
      ||| (y116 === var q9 &&& elemoEvaloNandoNandoNando y115 y117 q9) )
  and evaloEvaloNandoNando y122 y123 y125 y127 y128 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y127 === conj q1 q2
      &&& (evaloEvaloNandoNando y122 y123 y125 q1 q3 &&& evaloNandoNando y128 y122 q2 q3)
      ||| (y127 === disj q1 q2 &&& (_evaloEvaloNandoNando y122 q1 q2 q4 q5 &&& _evaloNandoNando !!true y122 y123 y125 q6 &&& _nando q4 q5 y128))
      ||| (y127 === neg q1 &&& (evaloEvaloNandoNando y122 y123 y125 q1 q3 &&& _nando q3 q3 y128))
      ||| (y127 === var q7 &&& elemoEvaloNandoNando y122 y123 y125 y128 q7) )
  and elemoEvaloNandoNando y129 y130 y132 y134 y135 =
    fresh (q1 q2 q3 q4)
      ( y135 === Nat.zero
      &&& (y129 === ( % ) y134 q1)
      &&& _evaloNandoNando !!true (( % ) y134 q1) y130 y132 q2
      ||| (y135 === Nat.succ q3 &&& (y129 === ( % ) q4 q1) &&& (_evaloNandoNando !!true (( % ) q4 q1) y130 y132 q2 &&& elemo y134 q3 q1)) )
  and _evaloEvaloNandoNando y136 y137 y138 y140 y142 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y137 === conj q1 q2
      &&& (evaloNandoNando q3 y136 q2 q4 &&& __evaloEvaloNando y136 y138 y142 q1 q4 &&& _nando q3 q3 y140)
      ||| (y137 === disj q1 q2 &&& (_evaloEvaloNandoNando y136 q1 q2 q5 q6 &&& _nandoNando q3 q3 y140 q5 q6 &&& _evaloNando y136 y138 q7 q7 y142))
      ||| (y137 === neg q1 &&& (_evaloNandoNando y140 y136 q1 q3 q3 &&& _evaloNando y136 y138 q7 q7 y142))
      ||| (y137 === var q8 &&& _elemoEvaloNandoNando y136 y138 y140 y142 q8) )
  and __evaloEvaloNando y143 y144 y146 y147 y148 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y147 === conj q1 q2
      &&& (evaloNandoNando y148 y143 q2 q3 &&& __evaloEvaloNando y143 y144 y146 q1 q3)
      ||| (y147 === disj q1 q2 &&& (_evaloEvaloNandoNando y143 q1 q2 q4 q5 &&& _evaloNando y143 y144 q6 q6 y146 &&& _nando q4 q5 y148))
      ||| (y147 === neg q1 &&& (__evaloEvaloNando y143 y144 y146 q1 q3 &&& _nando q3 q3 y148))
      ||| (y147 === var q7 &&& _elemoEvaloNando y143 y144 y146 y148 q7) )
  and _elemoEvaloNando y149 y150 y152 y153 y154 =
    fresh (q1 q2 q3 q4)
      ( y154 === Nat.zero
      &&& (y149 === ( % ) y153 q1)
      &&& _evaloNando (( % ) y153 q1) y150 q2 q2 y152
      ||| (y154 === Nat.succ q3 &&& (y149 === ( % ) q4 q1) &&& (_evaloNando (( % ) q4 q1) y150 q2 q2 y152 &&& elemo y153 q3 q1)) )
  and _elemoEvaloNandoNando y155 y156 y158 y160 y161 =
    fresh (q1 q2 q3 q4 q5)
      ( y161 === Nat.zero
      &&& (y155 === ( % ) q1 q2)
      &&& __evaloNandoNando y156 y158 y160 q1 q2
      ||| (y161 === Nat.succ q3 &&& (y155 === ( % ) q4 q2) &&& (elemoNando q2 q1 q1 y158 q3 &&& _evaloNando (( % ) q4 q2) y156 q5 q5 y160)) )
  and __evaloNandoNando y162 y163 y165 y166 y167 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y162 === conj q1 q2
      &&& (evaloNandoNando q3 (( % ) y166 y167) q2 q4 &&& __evaloNando y163 y166 y167 q1 q4 &&& _nando q3 q3 y165)
      ||| (y162 === disj q1 q2 &&& (_evaloEvaloNandoNando (( % ) y166 y167) q1 q2 q5 q6 &&& _nandoNando q3 q3 y165 q5 q6 &&& _nando y166 y166 y163))
      ||| (y162 === neg q1 &&& (___evaloNandoNando y163 y166 y167 q1 q4 q4 q3 &&& _nando q3 q3 y165))
      ||| (y162 === var q7 &&& ___elemoNandoNando y163 y165 y166 y167 q7) )
  and __evaloNando y168 y169 y170 y171 y172 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y171 === conj q1 q2
      &&& (evaloNandoNando y172 (( % ) y169 y170) q2 q3 &&& __evaloNando y168 y169 y170 q1 q3)
      ||| (y171 === disj q1 q2 &&& (_evaloEvaloNandoNando (( % ) y169 y170) q1 q2 q4 q5 &&& _nando q4 q5 y172 &&& _nando y169 y169 y168))
      ||| (y171 === neg q1 &&& ___evaloNandoNando y168 y169 y170 q1 q3 q3 y172)
      ||| (y171 === var q6 &&& _elemoNando y168 y169 y170 y172 q6) )
  and ___evaloNandoNando y173 y174 y175 y176 y177 y178 y179 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y176 === conj q1 q2
      &&& (evaloNandoNando y178 (( % ) y174 y175) q2 q3 &&& __evaloNando y173 y174 y175 q1 q3 &&& _nando y177 y178 y179)
      ||| (y176 === disj q1 q2 &&& (_evaloEvaloNandoNando (( % ) y174 y175) q1 q2 q4 q5 &&& _nandoNando y177 y178 y179 q4 q5 &&& _nando y174 y174 y173))
      ||| (y176 === neg q1 &&& _evaloNandoNandoNando y173 y174 y175 y179 q1 y177 y178)
      ||| (y176 === var q6 &&& __elemoNandoNando y173 y174 y175 y177 y178 y179 q6) )
  and __elemoNandoNando y180 y181 y182 y183 y184 y185 y186 =
    fresh (q1)
      ( y186 === Nat.zero &&& (y184 === y181) &&& ___nandoNando y180 y183 y185 y181
      ||| (y186 === Nat.succ q1 &&& (elemoNando y182 y183 y184 y185 q1 &&& _nando y181 y181 y180)) )
  and ___nandoNando y187 y188 y189 y190 =
    y188 === !!false &&& (y190 === !!false) &&& (y189 === !!true) &&& _nando !!false !!false y187
    ||| (y188 === !!false &&& (y190 === !!true) &&& (y189 === !!true) &&& _nando !!true !!true y187)
    ||| (y188 === !!true &&& (y190 === !!false) &&& (y189 === !!true) &&& _nando !!false !!false y187)
    ||| (y188 === !!true &&& (y190 === !!true) &&& (y189 === !!false) &&& _nando !!true !!true y187)
  and _evaloNandoNandoNando y191 y192 y193 y194 y195 y197 y198 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y195 === conj q1 q2
      &&& (evaloNandoNando q3 (( % ) y192 y193) q2 q4 &&& _nandoNando y197 y198 y194 q3 q3 &&& __evaloNando y191 y192 y193 q1 q4)
      ||| ( y195 === disj q1 q2
          &&& (_evaloEvaloNandoNando (( % ) y192 y193) q1 q2 q5 q6 &&& __nandoNandoNando y194 y197 y198 q5 q6 &&& _nando y192 y192 y191) )
      ||| (y195 === neg q1 &&& (evaloNandoNandoNando (( % ) y192 y193) y197 y198 y194 q1 q3 q3 &&& _nando y192 y192 y191))
      ||| (y195 === var q7 &&& _elemoNandoNandoNando y191 y192 y193 y194 y197 y198 q7) )
  and _elemoNandoNandoNando y199 y200 y201 y202 y204 y205 y206 =
    fresh (q1)
      ( y206 === Nat.zero &&& ___nandoNandoNando y199 y202 y204 y205 y200
      ||| (y206 === Nat.succ q1 &&& (_elemoNandoNando y202 y201 y204 y205 q1 &&& _nando y200 y200 y199)) )
  and ___nandoNandoNando y207 y208 y209 y210 y211 =
    y211 === !!false &&& (y210 === !!true)
    &&& (_nando y209 !!true y208 &&& _nando !!false !!false y207)
    ||| (y211 === !!true &&& (y210 === !!false) &&& (_nando y209 !!false y208 &&& _nando !!true !!true y207))
  and _elemoNando y212 y213 y214 y215 y216 =
    fresh (q1)
      (y216 === Nat.zero &&& (y215 === y213) &&& _nando y213 y213 y212 ||| (y216 === Nat.succ q1 &&& (elemo y215 q1 y214 &&& _nando y213 y213 y212)))
  and ___elemoNandoNando y217 y219 y220 y221 y222 =
    fresh (q1 q2)
      (y222 === Nat.zero &&& ___nandoNando y219 y220 y217 y220 ||| (y222 === Nat.succ q1 &&& (elemoNando y221 q2 q2 y219 q1 &&& _nando y220 y220 y217)))
  and elemoEvaloNandoNandoNando y223 y224 y229 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y229 === Nat.zero
      &&& (y223 === ( % ) q1 q2)
      &&& __evaloNandoNandoNando y224 q1 q2
      ||| (y229 === Nat.succ q3 &&& (y223 === ( % ) q4 q2) &&& (_____elemoNandoNando q5 q3 q2 &&& _evaloNando (( % ) q4 q2) y224 q6 q6 q5)) )
  and __evaloNandoNandoNando y230 y234 y235 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y230 === conj q1 q2
      &&& (evaloNandoNando q3 (( % ) y234 y235) q2 q4 &&& ____evaloNandoNando q5 y234 y235 q1 q4 &&& _nando q3 q3 q5)
      ||| (y230 === disj q1 q2 &&& (_evaloEvaloNandoNando (( % ) y234 y235) q1 q2 q6 q7 &&& __nandoNandoNando !!true q8 q5 q6 q7 &&& _nando y234 y234 q8))
      ||| (y230 === neg q1 &&& (evaloNandoNandoNando (( % ) y234 y235) q8 q5 !!true q1 q3 q3 &&& _nando y234 y234 q8))
      ||| (y230 === var q9 &&& __elemoNandoNandoNando y234 y235 q9) )
  and ____evaloNandoNando y237 y238 y239 y240 y241 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y240 === conj q1 q2
      &&& (evaloNandoNando y241 (( % ) y238 y239) q2 q3 &&& ____evaloNandoNando y237 y238 y239 q1 q3)
      ||| (y240 === disj q1 q2 &&& (_evaloEvaloNandoNando (( % ) y238 y239) q1 q2 q4 q5 &&& __nandoNando !!true y238 q6 y237 &&& _nando q4 q5 y241))
      ||| (y240 === neg q1 &&& (___evaloNandoNando q6 y238 y239 q1 q3 q3 y241 &&& _nando q6 y237 !!true))
      ||| (y240 === var q7 &&& ____elemoNandoNando y237 y238 y239 y241 q7) )
  and ____elemoNandoNando y243 y244 y245 y246 y247 =
    fresh (q1 q2)
      ( y247 === Nat.zero &&& (y246 === y244) &&& __nandoNando !!true y244 q1 y243
      ||| (y247 === Nat.succ q2 &&& (__nandoNando !!true y244 q1 y243 &&& elemo y246 q2 y245)) )
  and __elemoNandoNandoNando y251 y252 y253 =
    fresh (q1 q2 q3)
      (y253 === Nat.zero &&& ____nandoNandoNando y251 ||| (y253 === Nat.succ q1 &&& (_elemoNandoNando !!true y252 q2 q3 q1 &&& _nando y251 y251 q2)))
  and ____nandoNandoNando y256 =
    fresh (q1) (y256 === !!false &&& _nandoNando !!true q1 !!true !!false !!false ||| (y256 === !!true &&& _nandoNando !!false q1 !!true !!true !!true))
  and _____elemoNandoNando y259 y260 y261 =
    fresh (q1 q2 q3 q4 q5)
      ( y260 === Nat.zero
      &&& (y261 === ( % ) q1 q2)
      &&& __nandoNando !!true q1 q3 y259
      ||| (y260 === Nat.succ q4 &&& (y261 === ( % ) q5 q2) &&& _____elemoNandoNando y259 q4 q2) )
  in
  evalo x0 x1