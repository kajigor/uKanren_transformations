fail() :- fail().
flipflip(Xt, Yt) :- flip(Tt, Yt), flip(Xt, Tt).
flip(leaf(X), leaf(X)).
flip(tree(Xt, Info, Yt), tree(Fyt, Info, Fxt)) :- flip(Xt, Fxt), flip(Yt, Fyt).