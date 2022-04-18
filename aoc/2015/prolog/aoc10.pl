:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- set_prolog_flag(double_quotes,chars).

digitlist([Digit|Ds]) --> [D],
                          { char_type(D, digit(Digit)) }, !,
                          digitlist(Ds).
digitlist([]) --> [].

pair_list(Char-N, [N,Char]).

look_and_say(Look, Say) :-
    clumped(Look, Pairs),
    maplist(pair_list, Pairs, Lists),
    flatten(Lists, Say).

iterate(_, 0, ArgN, ArgN) :- !.
iterate(Goal, N, Arg0, ArgN) :-
    call(Goal, Arg0, Arg1),
    N1 #= N-1, !,
    iterate(Goal, N1, Arg1, ArgN).

sol10a(A) :- phrase(digitlist(Ds), "1113122113"),
             iterate(look_and_say, 40, Ds, LS40), length(LS40, A).

sol10b(A) :- phrase(digitlist(Ds), "1113122113"),
             iterate(look_and_say, 50, Ds, LS40), length(LS40, A).
