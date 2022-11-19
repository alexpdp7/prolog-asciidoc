:- begin_tests(document).

:- set_prolog_flag(double_quotes, chars).

:- use_module(document).

test(doc_with_header) :- phrase(document(X), ":foo:\n\n* a\n"), !,
		      	 assertion(X == doc(h([attr_en([:], [f, o, o], nothing, [:, '\n'])], '\n'), [uol([uoli([*], [' '], [a, '\n'])])])).

test(doc_without_header) :- phrase(document(X), "* a\n"), !,
	                    assertion(X == doc(h(empty, empty), [uol([uoli([*], [' '], [a, '\n'])])])).

:- end_tests(document).
