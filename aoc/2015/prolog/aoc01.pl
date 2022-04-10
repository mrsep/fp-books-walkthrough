:- use_module(library(dif)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

sum(   [],0).
sum([H|T],S) :- S #= H+S1, sum(T,S1).

cum_sum_helper(   [],   _,        []).
cum_sum_helper([H|T], Acc, [AccN|TS]) :-
    AccN #= Acc+H, cum_sum_helper(T, AccN, TS).

cum_sum(   [],[]).
cum_sum([H|T], [H|L]) :- cum_sum_helper(T, H, L).

% reimplemented due to bug in scryer prolog nth1
nth0(0, [ E| _], E).
nth0(I, [E0|Es], E) :-
    dif(E0, E),
    I1 #= I-1,
    nth0(I1,Es,E).

nth1(I, Es, E) :-
    I0 #= I-1,
    nth0(I0, Es, E).

c2i('(',1).
c2i(')',-1).

sol01(L) :- phrase_from_file(seq(Chars), '../day01.txt'),
            maplist(c2i,Chars,L).

sol01a(X) :- sol01(L), sum(L,X).

sol01b(I) :- sol01(L), cum_sum(L, CL), nth1(I, CL, -1).
