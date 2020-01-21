all:
	idris --build idrlisp.ipkg

test:
	idris --testpkg idrlisp.ipkg

repl:
	idris --repl idrlisp.ipkg
