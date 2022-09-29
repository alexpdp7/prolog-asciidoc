% See https://docs.asciidoctor.org/asciidoc/latest/document/header/

:- module(header, [attribute_entry/3, header/3]).

:- set_prolog_flag(double_quotes, chars).

:-use_module(char_types).


header(h(X, Y)) --> header_lines(X), new_line(Y).
header_lines([X|XS]) --> header_line(X), header_lines(XS).
header_lines([]) --> [].
header_line(X) --> attribute_entry(X).

attribute_entry(attr_en(":", AN, nothing, ":\n")) --> ":", attribute_name(AN), ":\n".
attribute_name([F|R]) --> word_character(F), rest_of_attribute_name(R).
rest_of_attribute_name([F|R]) --> (word_character(F); hyphen(F)), rest_of_attribute_name(R).
rest_of_attribute_name([]) --> [].
