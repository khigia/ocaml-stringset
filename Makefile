TARGET_TEST=$(patsubst test/%Test.ml,test/%Test.native,$(wildcard test/*Test.ml))
TARGET_BIN=$(patsubst %Ex.ml,%Ex.native,$(wildcard *Ex.ml))

TARGETS=$(TARGET_TEST) $(TARGET_BIN)

OCAMLBUILD=ocamlbuild -classic-display -no-links

.PHONY: all
all: build symlnk

.PHONY: build
build:
	$(OCAMLBUILD) $(TARGETS)

.PHONY: symlnk
symlnk:
	mkdir -p bin
	for I in $(TARGET_BIN) ; do\
	    ln -f -s ../_build/$$I bin/$$I ; \
	done

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	rm -rf bin

.PHONY: utest
utest:
	@for I in `ls _build/test/*.native` ; do \
		./$$I ; \
	done
