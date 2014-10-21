P?=exemples/*

all: toy_interp top_top

toy_interp: toy.native

toy_top: itoy.top

runtop:
	rlwrap ./itoy.top -I _build

test: toy.native
	./$< $(P)

clean:
	ocamlbuild -clean

doc:
	ocamlbuild -docflags -charset,utf-8 toy.docdir/index.html

demo:
	@echo ./demo is a script. You don\'t need to build it.

.PHONY: all toy_interp toy_top runtop test clean doc demo
	
toy.native:
	ocamlbuild $@

itoy.top:
	ocamlbuild $@
