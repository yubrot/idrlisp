all:
	idris --build idrlisp.ipkg

test:
	idris --testpkg idrlisp.ipkg

clean:
	idris --clean idrlisp.ipkg

