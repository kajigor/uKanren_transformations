ocamlfind ocamlopt -rectypes -o run -package benchmark -package ocanren -package GT -syntax camlp5o -package GT.syntax.all -package ocanren.syntax -linkpkg tester.ml lorry.ml lorry_run.ml

