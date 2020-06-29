SOURCES=$(basename $(wildcard *.tex))

all: $(SOURCES)

$(SOURCES):
	xelatex --shell-escape $@.tex
	pdfcrop $@.pdf
	rm $@.pdf

clean:
	rm -f *.log *.aux *~
