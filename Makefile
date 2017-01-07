.PHONY: test apps build
all: build

build:
	ocaml pkg/pkg.ml build

test: build
	ocamlbuild -I test testwidget.native
	ocamlbuild -I test testwidget_lwt.native
	ocamlbuild -I test testsim.native
	ocamlbuild -I test testsim_lwt.native

apps:
	ocamlbuild -I apps wavedraw.native
	ocamlbuild -I apps waveterm.native

clean:
	ocamlbuild -clean

