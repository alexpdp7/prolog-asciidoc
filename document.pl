:- module(document, [document/3]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(header).
:- use_module(blocks).

document(doc(H, BS)) --> header(H), blocks(BS).
