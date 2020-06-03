ocamlfind ocamlopt -rectypes -o run -package benchmark -package ocanren -package GT -syntax camlp5o -package GT.syntax.all -package ocanren.syntax -linkpkg helper.ml rep20.ml rep1.ml rep50Norm.ml rep75Norm.ml rep50.ml rep75.ml rep100.ml rep100Norm.ml rep50NormExtra.ml run.ml

