TARGET_TEST=$(patsubst test/%Test.ml,test/%Test.native,$(wildcard test/*Test.ml))
TARGET_APP=$(patsubst %.ml,%.native,$(wildcard app/*.ml))
TARGET_EX=$(patsubst %.ml,%.native,$(wildcard ex/*.ml))
TARGET_DEV=$(patsubst %.ml,%.native,$(wildcard test/dev/*.ml))

TARGETS=$(TARGET_TEST) $(TARGET_EX) $(TARGET_APP)

OCAMLBUILD=ocamlbuild -classic-display -no-links

.PHONY: build
build:
	$(OCAMLBUILD) $(TARGETS)
	@mkdir -p bin/ex
	@for I in $(TARGET_EX) ; do\
	    ln -f -s ../../_build/$$I bin/ex/`basename $$I` ; \
	done
	@mkdir -p bin/app
	@for I in $(TARGET_APP) ; do\
	    ln -f -s ../../_build/$$I bin/app/`basename $$I` ; \
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

.PHONY: dev
dev:
	$(OCAMLBUILD) $(TARGET_DEV)
	@mkdir -p bin/dev
	@for I in $(TARGET_DEV) ; do\
	    ln -f -s ../../_build/$$I bin/dev/`basename $$I` ; \
	done
