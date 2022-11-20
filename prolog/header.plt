:- begin_tests(header).

:- set_prolog_flag(double_quotes, chars).

:- use_module(header).

test(empty_attribute_entry) :- phrase(attribute_entry(X), ":hola:\n"), !,
			       assertion(X == attr_en(":", [h, o, l, a], nothing, ":\n")).

test(header) :- phrase(header(X), ":hola:\n\n"), !,
		assertion(X = h([attr_en([:],[h,o,l,a],nothing,[:,'\n'])],['\n'])).

:- end_tests(header).
