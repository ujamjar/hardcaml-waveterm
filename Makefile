.PHONY: test

test:
	ocamlbuild -I test testwidget.native
	ocamlbuild -I test testwidget_lwt.native
	ocamlbuild -I test testsim.native
	ocamlbuild -I test testsim_lwt.native
