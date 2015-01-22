# Waveform viewer library for HardCaml

A text (unicode) based digital waveform viewer library.

* Lambda-term based interactive viewer app
* HTML output (no fancy canvas needed - just \<pre\> and html escape codes)
* UTF-8 output (for UTF aware terminals and editors with optional ANSI styling)

# Examples

* [UTF-8 output](https://raw.githubusercontent.com/ujamjar/hardcaml-wave-term/master/test/wave.txt)

```
$ wavedraw -o test/wave.txt data.wave
```
* [Basic HTML output](http://www.ujamjar.com/hardcaml/wave-term/wave-static.html)

```
$ wavedraw -html static -o test/wave-static.html data.wave 
```

* [Styled HTML output with scrolling](http://www.ujamjar.com/hardcaml/wave-term/wave-scroll.html)

```
$ wavedraw -html scroll -o test/wave-scroll.html data.wave 
```

# TODO

## General

* [x] connect to HardCaml Bits type 
* [x] simplify wave type functor
* [x] configure data display
* [x] split library for javascript mode (lambda-term deps in seperate package)
* [x] sub cycle display (scaling factor < 1)
* [ ] cursor display
* [ ] pick; mouse coord -> wave cycle translation

## Interactive app

* [x] connect to testbench
* [x] Lwt based interactive viewing
* [ ] scroll names/values windows
* [ ] resize sub-windows
* [ ] interactive testbench driver mode (edit signal values, send back to simulation)
* [ ] mouse interaction

## Static drawing

* [x] set start cycle (horizontal)
* [ ] set start wave (vertical)
* [x] load data from file

