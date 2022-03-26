:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(library(pio)).

sum(   [],0).
sum([H|T],S) :- S #= H+S1, sum(T,S1).

cum_sum_helper(   [],   _,        []).
cum_sum_helper([H|T], Acc, [AccN|TS]) :-
    AccN #= Acc+H, cum_sum_helper(T, AccN, TS).

cum_sum(   [],[]).
cum_sum([H|T], [H|L]) :- cum_sum_helper(T, H, L).

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

c2i('(',1).
c2i(')',-1).

sol01(L) :- phrase_from_file(seq(Codes), '../day01.txt'),
            string_codes(Str,Codes), string_chars(Str,Chars),
            maplist(c2i,Chars,L).

sol01a(X) :- sol01(L), sum(L,X).

sol01b(I) :- sol01(L), cum_sum(L, CL), nth1(I, CL, -1).
