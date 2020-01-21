idrlisp
==

## Under construction

idrlisp is a [Idris](https://www.idris-lang.org/) version of [ocalisp](https://github.com/yubrot/ocalisp), a tiny Lisp-1 implementation.

## String support limitation

Because Idris's `Char` is backend dependent, additional work is required to handle strings as UTF-8 byte sequences. For now, some kind of builtins like `str-bytesize` behaves inappropriately with non-ASCII characters.

