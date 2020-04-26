MODULES=board command common state main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip snakes-and-ladders.zip *.ml* *.json _tags Makefile
	
clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private sandn.zip

install:
	opam install ANSITerminal yojson oUnit
