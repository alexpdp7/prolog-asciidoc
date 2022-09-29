:- module(char_types, [punct/3, space/3, hyphen/3, word_character/3, new_line/3]).

:- set_prolog_flag(double_quotes, chars).

% required in scryer
:- use_module(library(charsio)). 

punct(',') --> ",".
punct(';') --> ";".
punct('"') --> "\"".
punct('.') --> ".".
punct('?') --> "?".
punct('!') --> "!".

space(' ') --> " ".

hyphen('-') --> "-".

word_character(F) --> [F], {char_type(F, alnum)}.
word_character("_") --> "_".

new_line('\n') --> "\n".
