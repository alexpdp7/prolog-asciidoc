:- begin_tests(unordered_list).

:- set_prolog_flag(double_quotes, chars).

:- use_module(blocks).

test(unordered_list) :- phrase(unordered_list(X), "* a\n* b\n* c\n"), !,
			assertion(X == [uoli([*], [' '], [a], ['\n']), uoli([*], [' '], [b], ['\n']), uoli([*], [' '], [c], ['\n'])]).

:- end_tests(unordered_list).
