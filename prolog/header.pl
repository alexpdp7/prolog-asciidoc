% See https://docs.asciidoctor.org/asciidoc/latest/document/header/

:- module(header, [attribute_entry/3, header/3, header_sep/3]).

:- set_prolog_flag(double_quotes, chars).

:-use_module(char_types).

header(h(X, Y)) --> header_lines(X), header_sep(Y).
header_lines([X|XS]) --> header_line(X), header_lines(XS).
header_lines([]) --> [].
header_line(X) --> attribute_entry(X).

header_sep([]) --> [].
header_sep([X|XS]) --> new_line(X), header_sep(XS).

attribute_entry(attr_en(":", AN, nothing, ":\n")) --> ":", attribute_name(AN), ":\n".
attribute_name([F|R]) --> word_character(F), rest_of_attribute_name(R).
rest_of_attribute_name([F|R]) --> (word_character(F); hyphen(F)), rest_of_attribute_name(R).
rest_of_attribute_name([]) --> [].
