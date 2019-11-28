SUBDIRS = parse,top,util,type,dynamics
LIBS = str
MAIN = top

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind

DEBUG = $(MAIN).d.byte
NATIVE = $(MAIN).native
TOPLEVEL = $(MAIN).top


default: all

bin:
	mkdir bin

all: lat

lat: ./bin/lat
./bin/lat: native
	mkdir -p ./bin
	install $(NATIVE) $@

debug: $(DEBUG)
native: $(NATIVE)
top: $(TOPLEVEL)

OCAMLOPTS = $(OCAMLBUILDFLAGS) -Is $(SUBDIRS) -libs $(LIBS)
OCAMLBRUN = $(OCAMLBUILD) $(OCAMLOPTS)

%.native: always
	$(OCAMLBRUN) $@
%.d.byte: always
	$(OCAMLBRUN) -cflags -annot $@
top: always
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo			# $(OCAMLBUILD) -clean needs a newline
	rm -f ./bin/lat

always:

.PHONY: lat debug native top clean always
