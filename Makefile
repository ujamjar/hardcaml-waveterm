SRC = \
	gfx.mli gfx.ml \
	gfx_lterm.mli gfx_lterm.ml \
	gfx_html.mli gfx_html.ml \
	wave.mli wave.ml \
	render.mli render.ml \

waveterm: $(SRC) waveterm.ml
	ocamlfind c -g -syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		$(SRC) \
		waveterm.ml \
		-o waveterm

clean:
	rm -f *.cm[iox] *.o waveterm
