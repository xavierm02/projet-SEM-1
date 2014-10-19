all:
	ocamlbuild toy.native

clean:
	ocamlbuild -clean

top:
	ocamlbuild itoy.top
	rlwrap ./itoy.top -I _build

doc:
	ocamlbuild -docflags -charset,utf-8 toy.docdir/index.html

.PHONY: all clean top doc
	
