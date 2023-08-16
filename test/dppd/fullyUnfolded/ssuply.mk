:: ssuply snum pnum qty =
  [pname weight sname status city:
    {londonRedHeavyParts pnum pname weight} /\
    {bestSuppliers snum sname status city} /\
    {supply snum pnum qty}]

:: bestSuppliers snum sname status scity =
  [city:
    {goodSuppliers snum sname status scity} /\
    {goodCities city} /\
    city === scity]

:: goodSuppliers snum sname status city =
  [ten:
    ten === succ succ succ succ succ succ succ succ succ succ zero /\
    {suppliers snum sname status city} /\
    {lto status ten trueo}]


:: londonRedHeavyParts pnum pname weight =
  [pno colour:
    {londonRedParts pnum pname weight} /\
    {londonHeavyWeights pno pname colour weight} /\
    pnum === pno
  ]


:: londonRedParts pnum pname weight =
  [city:
    {redParts pnum pname weight city} /\
    city === <london:>
  ]

:: redPargs pnum pname weight city =
  [colour:
    {parts pnum pname colour weight city} /\
    colour === <red:>
  ]

:: londonHeavyWeights pnum pname colour weight =
  [city:
    {heavyWeights pnum pname colour weight city} /\
    city === <london:>]

:: heavyWeights pnum pname colour weight city =
  [ten:
    ten === succ succ succ succ succ succ succ succ succ succ zero /\
    {parts pnum pname colour weight city} /\
    {lto weight ten trueo}]

:: heavyWeights pnum pname colour weight city =
  [ten:
    ten === succ succ succ succ succ succ succ succ succ succ zero /\
    {parts pnum pname colour weight city} /\
    {lto weight ten trueo}]

:: goodCities city = conde
  (city === <paris:>)
  (city === <london:>)
  (city === <hongkong:>)
  (city === <regina:>)
  (city === <saskatoon:>)

:: supply s p w = conde
  (s === <s1:> /\ p === <p1:> /\ w === 300)
  (s === <s1:> /\ p === <p2:> /\ w === 200)
  (s === <s1:> /\ p === <p3:> /\ w === 400)
  (s === <s2:> /\ p === <p1:> /\ w === 300)
  (s === <s2:> /\ p === <p2:> /\ w === 400)
  (s === <s3:> /\ p === <p1:> /\ w === 400)
  (s === <s4:> /\ p === <p1:> /\ w === 200)
  (s === <s5:> /\ p === <p1:> /\ w === 500)
  (s === <s5:> /\ p === <p2:> /\ w === 400)

:: parts p type colour num city = conde
  (p === <p1:> /\ type === <nut:> /\ colour === <red:> /\ num === 12 /\ city === <london:>)
  (p === <p2:> /\ type === <bolt:> /\ colour === <green:> /\ num === 17 /\ city === <paris:>)
  (p === <p3:> /\ type === <screw:> /\ colour === <blue:> /\ num === 17 /\ city === <rome:>)

:: suppliers s name num city = conde
  (s === <s1:> /\ name === <smith:> /\ num === 20 /\ city === <london:>)
  (s === <s2:> /\ name === <jones:> /\ num === 10 /\ city === <paris:>)
  (s === <s3:> /\ name === <blake:> /\ num === 30 /\ city === <paris:>)
  (s === <s4:> /\ name === <clark:> /\ num === 20 /\ city === <london:>)
  (s === <s5:> /\ name === <adams:> /\ num === 30 /\ city === <athens:>)

? {ssuply snum pnum qty}