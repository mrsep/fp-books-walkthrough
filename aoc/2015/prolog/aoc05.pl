:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).

% from clpz
include(Goal, Ls0, Ls) :-
    include_(Ls0, Goal, Ls).

include_([], _, []).
include_([L|Ls0], Goal, Ls) :-
    (   call(Goal, L) ->
        Ls = [L|Rest]
    ;   Ls = Rest
    ),
    include_(Ls0, Goal, Rest).

vowel('a').
vowel('e').
vowel('i').
vowel('o').
vowel('u').

vowels(0) --> ... .
vowels(N) --> {vowel(V), N1 #= N-1}, ..., [V], vowels(N1).

duplicate --> ..., [V,V], ... .

no_sub(BadSubString) --> ..., BadSubString, ..., !, {false}.
no_sub(_) --> ... .

has_nosub(String, BadSubString) :- phrase(no_sub(BadSubString), String).

has_no_bad_substring(String, BadSubStrings) :- maplist(has_nosub(String), BadSubStrings).

isNiceA(String) :-
    phrase(vowels(3), String),
    phrase(duplicate, String),
    has_no_bad_substring(String, ["ab", "cd", "pq", "xy"]).

sol05a(N) :- phrase_from_file(lines(Lines), '../day05.txt'),
             include(isNiceA, Lines, NiceLines),
             length(NiceLines, N).
           
doublepair --> ..., [V,W], ..., [V,W], ... .

interduplicate --> ..., [V,_,V], ... .

isNiceB(String) :-
    phrase(doublepair, String),
    phrase(interduplicate, String).

sol05b(N) :- phrase_from_file(lines(Lines), '../day05.txt'),
             include(isNiceB, Lines, NiceLines),
             length(NiceLines, N).
