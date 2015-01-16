all: waveterm wavehtml waveutf8

SRC = \
	src/gfx.mli src/gfx.ml \
	src/gfx_lterm.mli src/gfx_lterm.ml \
	src/write.mli src/write.ml \
	src/wave.mli src/wave.ml \
	src/render.mli src/render.ml \

wave.cma: $(SRC)
	ocamlfind c -a -I src -g -package lambda-term $(SRC) -o wave.cma

waveterm: wave.cma test/data.ml test/waveterm.ml
	ocamlfind c -g -I src -I test \
		-syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		wave.cma test/data.ml test/waveterm.ml -o waveterm

wavehtml: wave.cma test/data.ml test/wavehtml.ml
	ocamlfind c -g -I src -I test \
		-syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		wave.cma test/data.ml test/wavehtml.ml -o wavehtml

waveutf8: wave.cma test/data.ml test/waveutf8.ml
	ocamlfind c -g -I src -I test \
		-syntax camlp4o -package lwt.syntax,lambda-term -linkpkg \
		wave.cma test/data.ml test/waveutf8.ml -o waveutf8

clean:
	rm -f src/*.cm[ioxa] test/*.cm[ioxa] wave.cma waveterm wavehtml waveutf8
	find . -name "*~" | xargs rm -f

