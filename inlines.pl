:- module(inlines, [parse_line/2]).

:- set_prolog_flag(double_quotes, chars).

% this is unnecessary with SWI, but needed with Scryer
:- use_module(library(lists)).

:-use_module(char_types).


formatting_mark("*") --> "*".
formatting_mark("_") --> "_".
formatting_mark("`") --> "`".
formatting_mark("#") --> "#".
formatting_mark("~") --> "~".

pre_constrained_formatting_mark(pre_cfm(X)) --> space(X).
pre_constrained_formatting_mark(pre_cfm(bl)) --> [bl].

post_constrained_formatting_mark(post_cfm(X)) --> space(X).
post_constrained_formatting_mark(post_cfm(X)) --> punct(X).
post_constrained_formatting_mark(post_cfm(X)) --> new_line(X).
post_constrained_formatting_mark(post_cfm(el)) --> [el].

constrained_formatting_mark([Pre, cfm(F, T, F)]), [Post] -->
    pre_constrained_formatting_mark(pre_cfm(Pre)),
    formatting_mark(F),
    nested_line_parts(T, F),
    {not_wrapped_in_spaces(T)},
    formatting_mark(F),
    post_constrained_formatting_mark(post_cfm(Post)).

not_wrapped_in_spaces(X) :- not_prefixed_by_spaces(X), reverse(X, RX), not_prefixed_by_spaces(RX).
not_prefixed_by_spaces([bl|X]) :- !, not_prefixed_by_spaces(X).
not_prefixed_by_spaces([' '|_]) :- !, fail.
not_prefixed_by_spaces(_).

% this is a hack to surround the content of a constrained formatting mark in be-el
nested_line_parts(X, F, B, A) :- append([B1, F, B2], B), append([[bl|B1], [el], F, B2], NB), line_parts(X, NB, A).

unconstrained_formatting_mark([ucfm(F, F, T, F, F)]) -->
    formatting_mark(F),
    formatting_mark(F),
    nested_line_parts(T,F),
    formatting_mark(F),
    formatting_mark(F).

char([T]) --> [T].

line_part(X) --> unconstrained_formatting_mark(X), !.
line_part(X) --> constrained_formatting_mark(X), !.
line_part(X) --> char(X), !.

line_parts_raw([X|XS]) --> line_part(X), line_parts_raw(XS).
line_parts_raw([]) --> [].

line_parts(XRFT, B, A) :- line_parts_raw(XR, B, A), append(XR, XRF), append([bl|XRFT], [el], XRF).

parse_line(X, Y) :- append([[bl|X], [el]], XW), phrase(line_parts(Y), XW).
