:- module(blocks, [blocks/3, block/3, unordered_list/3, paragraph/3, unordered_list_item/3]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(inlines).

blocks([]) --> [].
blocks([X]) --> block_sep(X).
blocks([X]) --> block(X).
blocks([X,Y|Z]) --> block(X), block_sep(Y), blocks(Z).

block_sep(bs("\n\n")) --> "\n\n".

block(uol(X)) --> unordered_list(X).
block(para(X)) --> paragraph(X).

% Unordered list

unordered_list([X, ls("\n")|XS]) --> unordered_list_item(X), "\n", unordered_list(XS).
unordered_list([X]) --> unordered_list_item(X).
unordered_list_item(uoli("*", " ", PT)) -->
    "* ",
    [X|XS],
    {parse_line([X|XS], PT)}.

paragraph(_) --> "* ", !, {fail}.
paragraph(PT) -->
    [X|XS],
    {parse_line([X|XS], PT)}.
