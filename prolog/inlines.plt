:- begin_tests(inlines).

:- set_prolog_flag(double_quotes, chars).
:- use_module(inlines).


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
test(cfm_no_constraint_left) :- parse_line("*a *", X), !,
				assertion(X == [*, a, ' ', *]).
test(cfm_no_constraint_right) :- parse_line("* a*", X), !,
				 assertion(X == [*, ' ', a, *]).

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
