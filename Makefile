all: waveterm wavehtml

SRC = \
	gfx.mli gfx.ml \
	gfx_lterm.mli gfx_lterm.ml \
	gfx_html.mli gfx_html.ml \
	wave.mli wave.ml \
	render.mli render.ml \

wave.cma: $(SRC)
	ocamlfind c -a -g -package lambda-term $(SRC) -o wave.cma

waveterm: wave.cma waveterm.ml
	ocamlfind c -g -syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		wave.cma waveterm.ml -o waveterm

wavehtml: wave.cma wavehtml.ml
	ocamlfind c -g -syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		wave.cma wavehtml.ml -o wavehtml

clean:
	rm -f *.cm[ioxa] *.o waveterm wavehtml
	find . -name "*~" | xargs rm -f
