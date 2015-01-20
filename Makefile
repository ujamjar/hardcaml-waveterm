
LIBS = _build/HardCamlWaveTerm.cma _build/HardCamlWaveLTerm.cma \
		   _build/HardCamlWaveTerm.cmxa _build/HardCamlWaveLTerm.cmxa 
APPS = waveterm.native wavedraw.native testsim.native

.PHONY: all libs apps

all: libs apps

libs: 
	ocamlbuild -use-ocamlfind HardCamlWaveTerm.cma
	ocamlbuild -use-ocamlfind HardCamlWaveLTerm.cma
	ocamlbuild -use-ocamlfind HardCamlWaveTerm.cmxa
	ocamlbuild -use-ocamlfind HardCamlWaveLTerm.cmxa

apps:
	ocamlbuild -use-ocamlfind $(APPS)

install:
	ocamlfind install hardcaml-waveterm META \
		_build/HardCamlWaveTerm.cmi _build/HardCamlWaveLTerm.cmi \
		_build/HardCamlWaveTerm.a _build/HardCamlWaveLTerm.a \
		$(LIBS)

uninstall:
	ocamlfind remove hardcaml-waveterm

clean:
	ocamlbuild -clean
	find . -name "*~" | xargs rm -f

