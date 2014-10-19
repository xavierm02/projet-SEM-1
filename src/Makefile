P?=exemples/fact.toy

all:
	ocamlbuild toy.native

test: all
	./toy.native $(P)

clean:
	ocamlbuild -clean

top:
	ocamlbuild itoy.top
	rlwrap ./itoy.top -I _build

doc:
	ocamlbuild -docflags -charset,utf-8 toy.docdir/index.html

.PHONY: all test clean top doc
	
