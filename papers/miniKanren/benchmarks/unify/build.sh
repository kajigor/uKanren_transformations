ocamlfind ocamlopt -rectypes -o run -package benchmark -package mtime -package mtime.clock.os -package ocanren -package GT -syntax camlp5o -package GT.syntax.all -package ocanren.syntax -linkpkg helper.ml original.ml conspd.ml cpd.ml branch.ml cpd_new.ml branch_new.ml run.ml

