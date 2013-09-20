BIC Beam Interpreted C code
===========================

BIC is first of all a C parser, it contains a lexer,
parser, linter and a preprocessor.

The code could be retargeted to anything you like,
but my main objective is to experniment with generating
erlang/beam code.

Extension that I have been thinking of to support interface
Erlang a bit better is to include the Erlang simple namespace
scheme. Thinking about it it may make sense to implement that
generally for the C language.

In other words allow calls like module:function(args)



