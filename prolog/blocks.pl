:- module(blocks, [blocks/3, block/3, unordered_list/3]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(inlines).

blocks([X|XS]) --> block(X), blocks(XS).
blocks([]) --> [].

block(uol(X)) --> unordered_list(X), !.

% Unordered list

unordered_list([X|XS]) --> unordered_list_item(X), unordered_list(XS).
unordered_list([X]) --> unordered_list_item(X).
unordered_list_item(uoli("*", " ", PT, "\n")) -->
    "* ",
    [X|XS],
    {parse_line([X|XS], PT), \+ append([_, "\n* ", _], PT)},
    "\n".
