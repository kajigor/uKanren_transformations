
build:
	cabal build
	./dist/build/uKanren-transformations/uKanren-transformations

compile:
	ocamlfind opt -rectypes -syntax camlp5o -package GT,ocanren,ocanren.syntax,ocanren.tester -o $(name).opt -linkpkg $(name)_pur.ml $(name)_run.ml

clean:
	rm -Rf *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *.out *~
