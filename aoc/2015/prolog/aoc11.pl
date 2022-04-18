:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- set_prolog_flag(double_quotes,chars).

% carry-increments integers in a list where each element is constrained to be in a given interval [Min,Sup)
incrange(Min, Sup, [A|As], [B|Bs]) :-
    A1 #= A+1, A1 #< Sup, !,
    A  #>= Min, A #< Sup, Min #< Sup,
    B  #= max(A1 mod Sup, Min),
    As=Bs, Max #= Sup - 1, maplist(between(Min,Max), As).
incrange(Min, Sup, [A|As], [B|Bs]) :-
    A1 #= A+1, A1 #>= Sup, !,
    A  #>= Min, A #< Sup, Min #< Sup,
    B  #= max(A1 mod Sup, Min),
    incrange(Min, Sup, As, Bs).
incrange(_, _, [], []).

% password conditions for its reverse sequence of integer character codes
valid(Ls) :-
    char_code('i', Ci), char_code('o', Co), char_code('l', Cl),
    valid_forbid([Ci,Co,Cl], Ls),
    valid_triple(Ls),
    valid_double(Ls).

valid_forbid([No|Noos], Ls) :-
    \+member(No, Ls), valid_forbid(Noos, Ls).
valid_forbid([], _).
valid_triple([L2,L1,L0|Ls]) :-
    (L1 #= L0+1, L2 #= L1+1), ! ; valid_triple([L1,L0|Ls]).
valid_double(Ls) :-
    clumped(Ls, Pairs),
    member(C1-N1,Pairs), N1 #> 1,
    member(C2-N2,Pairs), N2 #> 1,
    dif(C1,C2), !.

% iterate Goal until Pred is true
iterate_until(   _, Pred, ArgN, ArgN) :- call(Pred,ArgN).
iterate_until(Goal, Pred, Arg0, ArgN) :-
    \+call(Pred,Arg0), !,
    call(Goal, Arg0, Arg1),
    iterate_until(Goal, Pred, Arg1, ArgN).

sol11a(NewPassword) :- Password = "vzbxkghb",
                       char_code('a', Min), char_code('z', Max), Sup #= Max + 1,
                       string_codes(Password, Codes),
                       reverse(Codes, RevCodes),
                       iterate_until(incrange(Min, Sup), valid, RevCodes, NewRevCodes),
                       reverse(NewRevCodes, NewCodes),
                       string_codes(NewPassword, NewCodes).

sol11b(NewPassword) :- Password = "vzbxkghb",
                       char_code('a', Min), char_code('z', Max), Sup #= Max + 1,
                       string_codes(Password, Codes),
                       reverse(Codes, RevCodes),
                       iterate_until(incrange(Min, Sup), valid, RevCodes, TmpRevCodes),
                       incrange(Min, Sup, TmpRevCodes, NewTmpRevCodes),
                       iterate_until(incrange(Min, Sup), valid, NewTmpRevCodes, NewRevCodes),
                       reverse(NewRevCodes, NewCodes),
                       string_codes(NewPassword, NewCodes).

