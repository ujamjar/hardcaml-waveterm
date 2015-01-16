# Waveform viewer library for HardCaml

A text (unicode) based digital waveform viewer library.

* Lambda-term based interactive viewer app
* HTML output (no fancy canvas needed - just <pre> and html escape codes)
* UTF-8 output (for UTF aware terminals and editors with optional styling)

# Examples

* [HTML output with styling](test/index.html)

```
$ wavehtml > index.html # copy and paste <pre> section
```

* [UTF-8 without styling and different rendering parameters](test/wave.txt)

```
$ ./waveutf8 -nostyle -width 1 -height 1 -rows 10 -cols 60 > temp
```

