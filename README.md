BIC 
===

C parser and preprocessor written i Erlang.
Originally created for emulating C code but is
mainly used to parse and produce C-api header files.

Use simply like

    bic:file("fib.c")

will return declarations found in the C file

    -include:lib("bic/include/bic.hrl")
	
contains the record definitions needed to handle
the returned value.


    bic:command([AtomNamedFile])
	
Can be used to do simple tests from command line

