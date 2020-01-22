idrlisp
==

idrlisp is a [Idris](https://www.idris-lang.org/) version of [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) implementation.

    $ make
    $ ./idrlisp rosetta-lisp/examples/conways-gol.lisp

## String support limitation

Because Idris's `Char` is backend dependent, additional work is required to handle strings as UTF-8 byte sequences. For now, some kind of builtins like `str-bytesize` behaves inappropriately with non-ASCII characters.

