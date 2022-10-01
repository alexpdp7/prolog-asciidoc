:- module(document, [document/3, parse_file/2]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(header).
:- use_module(blocks).

document(doc(H, BS)) --> header(H), blocks(BS).

% from https://stackoverflow.com/a/65065773
read_file(File, Chars) :-
    open(File, read, Stream),
    get_char(Stream, Char),
    read_file(Stream, Char, Chars),
    close(Stream).

read_file(Stream, Char, Chars) :-
    (   Char == end_of_file ->
        Chars = []
    ;   Chars = [Char| Rest],
        get_char(Stream, Next),
        read_file(Stream, Next, Rest)
    ).

parse_file(F, P) :- read_file(F, C), phrase(document(P), C).
