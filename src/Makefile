P?=exemples/*

ML = $(wildcard *.ml)
MLI = $(wildcard *.mli)
MLL = $(wildcard *.mll)
MLY = $(wildcard *.mly)
SOURCES = $(ML) $(MLI) $(MLL) $(MLY)

all: toy_interp top_top

toy_interp: toy.native

toy_top: itoy.top

runtop: toy_top
	rlwrap ./itoy.top -I _build

test: toy.native
	./$< $(P)

clean:
	ocamlbuild -clean

doc: $(SOURCES)
	ocamlbuild -docflags -charset,utf-8 toy.docdir/index.html

demo:
	@echo ./demo is a script. You don\'t need to build it.

.PHONY: all toy_interp toy_top runtop test clean doc demo
	
toy.native: $(SOURCES) # for some reason, it only avoids rebuilding if you haven't modified your file since the last make clean
	ocamlbuild $@

itoy.top: $(SOURCES)
	ocamlbuild $@
