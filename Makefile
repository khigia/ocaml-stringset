TARGETS=\
	$(patsubst test/%Test.ml,test/%Test.native,$(wildcard test/*Test.ml))

OCAMLBUILD=ocamlbuild -classic-display

.PHONY: build
build:
	$(OCAMLBUILD) $(TARGETS)

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
