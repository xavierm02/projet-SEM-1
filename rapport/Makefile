SRC:=squelette.tex
OBJ:=$(SRC:%.tex=%.pdf)
LATEX = pdflatex

.SUFFIXES: .tex .pdf

.tex.pdf:
	$(LATEX) $< ; 
	@while ( grep -q "Rerun to get" $(SRC:%.tex=%.log)); do \
	$(LATEX) $< ; \
	done

all: $(OBJ)

.PHONY: all clean

clean:
	rm -rf *.{aux,log,pdf} *~
