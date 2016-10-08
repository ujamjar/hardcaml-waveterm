# Waveform viewer library for HardCaml

[![Build Status](https://travis-ci.org/ujamjar/hardcaml-waveterm.svg?branch=master)](https://travis-ci.org/ujamjar/hardcaml-waveterm)

A text (unicode) based digital waveform viewer library.

## Features

* `waveterm` an interactive viewer
* `wavedraw` render to text or html documents
* integrated with HardCaml simulations

## Examples

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

## TODO

### General

* [ ] Interactive javascript version
* [ ] check html rendering on different browsers (mobile chrome known dodgy)

## Interactive app

* [ ] resize sub-windows
* [ ] interactive testbench driver mode (edit signal values, send back to simulation)
