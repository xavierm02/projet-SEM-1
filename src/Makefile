all: toy.native

clean:
	ocamlbuild -clean

top: itoy.top
	rlwrap ./itoy.top -I _build

doc: toy.docdir

.PHONY: all clean top doc

toy.native: toy.ml
	ocamlbuild $@

itoy.top:
	ocamlbuild $@

toy.docdir:
	ocamlbuild -docflags -charset,utf-8 toy.docdir/index.html
