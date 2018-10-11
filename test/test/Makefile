# Run with make TOPFILE=<whatever you need>

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
SOURCES = appendo2.ml reverso.ml revacco.ml minmaxo.ml sorto.ml test.ml
CAMLP5 = -pp "camlp5o -I `ocamlfind -query ocanren.syntax` -I `ocamlfind -query GT.syntax` pa_minikanren.cmo pa_gt.cmo -L `ocamlfind -query GT.syntax`"
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -I `ocamlfind -query GT` -I `ocamlfind -query ocanren` #-I `~/prj/OCanren/samples`
OFLAGS = $(BFLAGS)

all: .depend $(TOPFILE).opt


.depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

$(TOPFILE).opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(TOPFILE).opt $(OFLAGS) GT.cmxa unix.cmxa MiniKanren.cmxa $(SOURCES:.ml=.cmx)

$(TOPFILE).byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(TOPFILE).byte $(BFLAGS) GT.cma unix.cma MiniKanren.cma  $(SOURCES:.ml=.cmo) #Sort.cmo

clean:
	rm -Rf *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~

-include .depend
# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

