all: waveterm wavedraw

SRC = \
	src/gfx.mli src/gfx.ml \
	src/gfx_lterm.mli src/gfx_lterm.ml \
	src/write.mli src/write.ml \
	src/wave.mli src/wave.ml \
	src/render.mli src/render.ml \

wave.cma: $(SRC)
	ocamlfind c -a -I src -g -package hardcaml,lambda-term $(SRC) -o wave.cma

waveterm: wave.cma test/waveterm.ml
	ocamlfind c -g -I src -I test \
		-syntax camlp4o -package hardcaml,lwt.syntax,lambda-term -linkpkg \
		wave.cma test/waveterm.ml -o waveterm

wavedraw: wave.cma test/wavedraw.ml
	ocamlfind c -g -I src -I test \
		-syntax camlp4o -package hardcaml,lwt.syntax,lambda-term -linkpkg \
		wave.cma test/wavedraw.ml -o wavedraw

clean:
	rm -f src/*.cm[ioxa] test/*.cm[ioxa] wave.cma waveterm wavedraw
	find . -name "*~" | xargs rm -f

