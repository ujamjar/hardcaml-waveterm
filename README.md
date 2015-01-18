# Waveform viewer library for HardCaml

A text (unicode) based digital waveform viewer library.

* Lambda-term based interactive viewer app
* HTML output (no fancy canvas needed - just \<pre\> and html escape codes)
* UTF-8 output (for UTF aware terminals and editors with optional ANSI styling)

# Examples

* [UTF-8 output](https://raw.githubusercontent.com/ujamjar/hardcaml-wave-term/master/test/wave.txt)

```
$ wavedraw > test/wave.txt
```
* [Basic HTML output](http://www.ujamjar.com/hardcaml/wave-term/wave-static.html)

```
$ wavedraw -html static > test/wave-static.html 
```

* [Styled HTML output with scrolling](http://www.ujamjar.com/hardcaml/wave-term/wave-scroll.html)

```
$ wavedraw -html static > test/wave-static.html 
```

# TODO

## General

* [ ] connect to HardCaml Bits type 
* [ ] simplify wave type functor
* [ ] configure data display
* [ ] split library for javascript mode (lambda-term deps in seperate package)

## Interactive app

* [ ] connect to testbench, read data as flow
* [ ] scroll names/values windows
* [ ] resize sub-windows
* [ ] interactive testbench driver mode (edit signal values, send back to simulation)

## Static drawing

* [ ] set start cycle (horizontal)
* [ ] set start wave (vertical)
* [ ] load data from file
