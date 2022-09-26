% Sample parsing of some AsciiDoc features
% Works on SWI Prolog, might work with Scryer Prolog
%
% Differences from AsciiDoctor:
%
% * _*a*_ is not "valid" AsciiDoctor:
%   * https://asciidoc.zulipchat.com/#narrow/stream/335219-asciidoc-lang/topic/Clarification.20on.20constrained.20formatting.20marks/near/299416275
%   * https://docs.asciidoctor.org/asciidoc/latest/text/bold/#mixing-bold-with-other-formatting

% run tests with:
%
% $ swipl -g run_tests -t halt asciidoc.pl

:- set_prolog_flag(double_quotes, chars).

% this is unnecessary with SWI, but needed with Scryer
:- use_module(library(lists)).

% char_type/2 is builtin in SWI, but in charsio in Scryer

:- use_module(library(charsio)). 

% scryer-prolog TODO:
% * find a way to run unit tests (they currently cause consulting this file to fail)

%%% Character types

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

%%% Document header

% See https://docs.asciidoctor.org/asciidoc/latest/document/header/

header(h(X, Y)) --> header_lines(X), new_line(Y).
header_lines([X|XS]) --> header_line(X), header_lines(XS).
header_lines([]) --> [].
header_line(X) --> attribute_entry(X).

attribute_entry(attr_en(":", AN, nothing, ":\n")) --> ":", attribute_name(AN), ":\n".
attribute_name([F|R]) --> word_character(F), rest_of_attribute_name(R).
rest_of_attribute_name([F|R]) --> (word_character(F); hyphen(F)), rest_of_attribute_name(R).
rest_of_attribute_name([]) --> [].

:- begin_tests(header).
:- set_prolog_flag(double_quotes, chars).

test(empty_attribute_entry) :- phrase(attribute_entry(X), ":hola:\n"), !,
			       assertion(X == attr_en(":", [h, o, l, a], nothing, ":\n")).

test(header) :- phrase(header(X), ":hola:\n\n"), !,
		X = h([attr_en([:], [h, o, l, a], nothing, [:, '\n'])], '\n').

:- end_tests(header).

%%% Inlines

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

:- begin_tests(inlines).
:- set_prolog_flag(double_quotes, chars).

test(plain) :- parse_line("abc", X), !,
	       assertion(X == [a, b, c]).
test(single_cfm) :- parse_line("*abc*", X), !,
		    assertion(X == [cfm([*], [a, b, c], [*])]).
test(nested_cfm) :- parse_line("*_a_*", X), !,
		    assertion(X == [cfm([*], [cfm(['_'], [a], ['_'])], [*])]).

test(double_nested_cfm) :- parse_line("*_a_ _b_*", X), !,
                           assertion(X == [cfm([*], [cfm(['_'], [a], ['_']), ' ', cfm(['_'], [b], ['_'])], [*])]).

test(cfm_no_constraint_outside) :- parse_line("a*b*", X), !,
				   assertion(X == [a, *, b, *]).
test(cfm_no_constraint_inside) :- parse_line("* a *", X), !,
				  assertion(X == [*, ' ', a, ' ', *]).

test(ucfm) :- parse_line("aaaaa**b**", X), !,
	      assertion(X == [a, a, a, a, a, ucfm([*], [*], [b], [*], [*])]).
test(ucfm_nested_in_cfm) :- parse_line("_aa**b**_", X), !,
			    assertion(X == [cfm(['_'], [a, a, ucfm([*], [*], [b], [*], [*])], ['_'])]).
test(lone_ucfm) :- parse_line("**b**", X), !,
		   assertion(X == [ucfm([*], [*], [b], [*], [*])]).
test(lone_ucfm_with_nested_cfm) :- parse_line("**_b_**", X), !,
				   assertion(X == [ucfm([*], [*], [cfm(['_'], [b], ['_'])], [*], [*])]).

test(consecutive_ucfms) :- parse_line("**a** **b**", X), !,
			   assertion(X == [ucfm([*], [*], [a], [*], [*]), ' ', ucfm([*], [*], [b], [*], [*])]).

:- end_tests(inlines).

%%% Blocks

block(uol(X)) --> unordered_list(X).

%%%% Unordered list

unordered_list([X|XS]) --> unordered_list_item(X), unordered_list(XS).
unordered_list([]) --> [].
unordered_list_item(uoli("*", " ", PT, "\n")) -->
    "* ",
    [X|XS],
    {parse_line([X|XS], PT), \+ append([_, "\n* ", _], PT)},
    "\n".

:- begin_tests(unordered_list).
:- set_prolog_flag(double_quotes, chars).

test(unordered_list) :- phrase(unordered_list(X), "* a\n* b\n* c\n"), !,
			assertion(X == [uoli([*], [' '], [a], ['\n']), uoli([*], [' '], [b], ['\n']), uoli([*], [' '], [c], ['\n'])]).

:- end_tests(unordered_list).
