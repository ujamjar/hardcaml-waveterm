.PHONY: test apps build
all: build

build:
	ocaml pkg/pkg.ml build

test: build
	ocamlbuild -I test testwidget.native
	ocamlbuild -I test testwidget_lwt.native
	ocamlbuild -I test testsim.native
	ocamlbuild -I test testsim_lwt.native

apps: build
	ocamlbuild -I apps wavedraw.native
	ocamlbuild -I apps waveterm.native

clean:
	ocamlbuild -clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

tag:
	git tag -a "v$(VERSION)" -m "v$(VERSION)."
	git push origin v$(VERSION)

prepare:
	opam publish prepare -r hardcaml $(NAME_VERSION) $(ARCHIVE)

publish:
	opam publish submit -r hardcaml $(NAME_VERSION)
	rm -rf $(NAME_VERSION)


