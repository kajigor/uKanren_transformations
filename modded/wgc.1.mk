eval 0[g -> g] 1[f -> g] 2[g -> g] =
  v.1[f -> g] == Nil && v.0[g -> g] == v.2[g -> g] ||
  eval v.5[f -> g] v.4[f -> g] v.2[g -> g] && step v.0[g -> g] v.3[f -> g] v.5[g -> g] && v.1[f -> g] == Cons v.3[g -> g] v.4[g -> g]
eval 0[f -> g] 1[f -> g] 2[g -> g] =
  v.1[f -> g] == Nil && v.0[f -> g] == v.2[g -> g] ||
  eval v.5[f -> g] v.4[f -> g] v.2[g -> g] && step v.0[f -> g] v.3[f -> g] v.5[g -> g] && v.1[f -> g] == Cons v.3[g -> g] v.4[g -> g]
step 0[g -> g] 1[f -> g] 2[g -> g] =
  v.0[g -> g] == State v.3[f -> g] v.4[f -> g] && step0 v.1[f -> g] v.2[g -> g]
step 0[f -> f] 1[f -> g] 2[g -> g] =
  v.0[f -> f] == State v.3[f -> f] v.4[f -> f] && step0 v.1[f -> g] v.2[g -> g]
step0 1[f -> g] 2[g -> g] =
  isMan v.3[f -> g] && noMan v.4[f -> g] && step' v.3[g -> g] v.4[g -> g] v.1[f -> g] v.2[g -> g] ||
  isMan v.4[f -> g] && noMan v.3[f -> g] && step00 v.1[f -> g] v.2[g -> g]
isMan 0[f -> f] =
  v.0[f -> f] == Side v.1[f -> f] v.2[f -> f] v.3[f -> f] v.33[f -> f] && v.33[f -> g] == Trueo
noMan 0[f -> f] =
  v.0[f -> f] == Side v.1[f -> f] v.2[f -> f] v.3[f -> f] v.34[f -> f] && v.34[f -> g] == Falso
step00 1[f -> g] 2[g -> g] =
  step' v.4[f -> g] v.3[f -> g] v.1[f -> g] v.5[f -> g] && swap v.5[g -> g] v.2[g -> g]
step' 0[f -> g] 1[f -> f] 2[f -> g] 3[f -> g] =
  v.0[f -> f] == Side v.5[f -> f] v.6[f -> f] v.7[f -> f] v.4[f -> f] && v.1[f -> f] == Side v.9[f -> f] v.10[f -> f] v.11[f -> f] v.8[f -> f] && step'0 v.2[f -> g] v.3[f -> g] v.0[f -> g]
step'0 2[f -> g] 3[f -> g] 0[f -> g] =
  safe v.3[f -> g] && v.3[g -> g] == State v.5[f -> g] v.7[f -> g] && v.5[g -> g] == Side v.5[g -> g] v.6[f -> g] v.7[g -> g] v.6[f -> g] && v.6[g -> g] == Falso && v.7[g -> g] == Side v.9[f -> g] v.10[f -> g] v.11[f -> g] v.8[f -> g] && v.8[g -> g] == Trueo && v.2[f -> g] == Empty ||
  v.2[f -> g] == Goat && isGoat v.0[f -> g] && step'00 v.3[f -> g] && safe v.3[g -> g] ||
  v.2[f -> g] == Wolf && isWolf v.0[f -> g] && step'00 v.3[f -> g] && safe v.3[g -> g] ||
  v.2[f -> g] == Cabbage && isCabbage v.0[f -> g] && step'00 v.3[f -> g] && safe v.3[g -> g]
isCabbage 0[f -> f] =
  v.0[f -> f] == Side v.1[f -> f] v.2[f -> f] v.31[f -> f] v.3[f -> f] && v.31[f -> g] == Trueo
safe 0[g -> g] =
  v.0[g -> g] == State v.1[f -> g] v.2[f -> g] && safe' v.1[g -> g] && safe' v.2[g -> g]
safe' 0[g -> g] =
  isMan v.0[g -> g] ||
  noMan v.0[g -> g] && safe'0 v.0[g -> g]
noMan 0[g -> g] =
  v.0[g -> g] == Side v.1[f -> g] v.2[f -> g] v.3[f -> g] v.34[f -> g] && v.34[g -> g] == Falso
safe'0 0[g -> g] =
  noGoat v.0[g -> g] ||
  isGoat v.0[g -> g] && safe'00 v.0[g -> g]
isGoat 0[g -> g] =
  v.0[g -> g] == Side v.27[f -> g] v.1[f -> g] v.2[f -> g] v.3[f -> g] && v.27[g -> g] == Trueo
safe'00 0[g -> g] =
  noCabbage v.0[g -> g] && noWolf v.0[g -> g] ||
  isCabbage v.0[g -> g] && isWolf v.0[g -> g]
isCabbage 0[g -> g] =
  v.0[g -> g] == Side v.1[f -> g] v.2[f -> g] v.31[f -> g] v.3[f -> g] && v.31[g -> g] == Trueo
isWolf 0[g -> g] =
  v.0[g -> g] == Side v.1[f -> g] v.29[f -> g] v.3[f -> g] v.4[f -> g] && v.29[g -> g] == Trueo
step'00 3[f -> f] =
  v.3[f -> f] == State v.21[f -> f] v.24[f -> f] && v.21[f -> f] == Side v.5[f -> f] v.6[f -> f] v.22[f -> f] v.23[f -> f] && v.22[f -> g] == Falso && v.23[f -> g] == Falso && v.24[f -> f] == Side v.9[f -> f] v.10[f -> f] v.25[f -> f] v.26[f -> f] && v.25[f -> g] == Trueo && v.26[f -> g] == Trueo
swap 0[g -> g] 1[g -> g] =
  v.0[g -> g] == State v.2[f -> g] v.3[f -> g] && v.1[g -> g] == State v.3[g -> g] v.2[g -> g]
eval v.0[g -> g] v.1[f -> g] v.2[g -> g]

