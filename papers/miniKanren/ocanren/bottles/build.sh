ocamlfind ocamlopt -rectypes -o run -package benchmark -package ocanren -package GT -syntax camlp5o -package GT.syntax.all -package ocanren.syntax -linkpkg helper.ml tester.ml bottles.ml bottles_trans.ml bottles_defer.ml bottles_fun.ml cpd.ml run.ml

