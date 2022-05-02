:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_prolog_flag(double_quotes,chars).

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( call(eol) | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

% parse line into a record
record(Capacity) --> number(Capacity).

line_term(String, Capacity) :-
    phrase(record(Capacity), String).

% multiply and sum for scalar product using foldl
sp(X, Y, V0, V1) :- V1 #= V0 + (X * Y).

mul(X,Y,M) :- M #= X*Y.

sum([], 0).
sum([L|Ls], S) :-
    S #= L + Ss,
    sum(Ls, Ss).

sol17(Amounts) :-
    Total #= 150,
    phrase_from_file(lines(CodeLines), '../day17.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, Caps),
    length(Caps, N),
    length(Amounts, N),
    Amounts ins 0..1,
    foldl(sp, Caps, Amounts, 0, Total).

sol17a(NSols) :-
    findall(Amounts, (sol17(Amounts), label(Amounts)), Solutions), length(Solutions, NSols).

sol17b(N, NSols) :-
    length(_, N),
    findall(Amounts, (sol17(Amounts),
                      sum(Amounts,N),
                      label(Amounts)), Solutions),
    length(Solutions, NSols),
    NSols #> 0, !.
