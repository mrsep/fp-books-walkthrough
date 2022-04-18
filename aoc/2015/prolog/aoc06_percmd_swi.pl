:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4,
                               del_assoc/4, get_assoc/3,
                               assoc_to_values/2]).

:- set_prolog_flag(double_quotes,chars).
:- set_prolog_flag(stack_limit, 2_147_483_648). 

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( call(eol) | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

% parse rectangular interval
corner(X,Y)       --> number(X), ",", number(Y).
rect(X0,Y0,X1,Y1) --> corner(X0,Y0), " through ", corner(X1,Y1), { X0 #=< X1, Y0 #=< Y1 }.

% parse commands
toggle(X0,Y0,X1,Y1) --> "toggle ", rect(X0,Y0,X1,Y1).
turn_on(X0,Y0,X1,Y1) --> "turn on ", rect(X0,Y0,X1,Y1).
turn_off(X0,Y0,X1,Y1) --> "turn off ", rect(X0,Y0,X1,Y1).

set_toggle(Assoc0, Assoc1, X, Y) :-
    (get_assoc(X-Y, Assoc0, V0), V0 #= 1) ->
        del_assoc(X-Y, Assoc0, V0, Assoc1);
    put_assoc(X-Y, Assoc0, 1, Assoc1). 

set_on(Assoc0, Assoc1, X, Y) :-
    put_assoc(X-Y, Assoc0, 1, Assoc1).

set_off(Assoc0, Assoc1, X, Y) :-
    get_assoc(X-Y, Assoc0, V) ->
        del_assoc(X-Y, Assoc0, V, Assoc1);
        Assoc0=Assoc1.

foldl_assoc(_, AssocN, AssocN, []) :- !.
foldl_assoc(Goal, Assoc0, AssocN, [L|Ls]) :-
    %format("~s\n",[L]), 
    call(Goal, Assoc0, Assoc1, L), !,
    foldl_assoc(Goal, Assoc1, AssocN, Ls).

foldl_rect(Goal, Assoc0, AssocN, Lx, Ly) :-
    foldl_rect_(Goal, Assoc0, AssocN, Lx, Lx, Ly).

foldl_rect_(_, AssocN, AssocN, _, _, []) :- !. % IMPORTANT CUT to remove choice point
foldl_rect_(Goal, Assoc0, AssocN, OLx, [], [_|Ly]) :-
    foldl_rect_(Goal, Assoc0, AssocN, OLx, OLx, Ly).
foldl_rect_(Goal, Assoc0, AssocN, OLx, [X|Lx], [Y|Ly]) :-
    call(Goal, Assoc0, Assoc1, X, Y), !,
    foldl_rect_(Goal, Assoc1, AssocN, OLx, Lx, [Y|Ly]).

% dispatch
command_bool(Assoc0, AssocN, String) :-
    phrase(toggle(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(set_toggle, Assoc0, AssocN, Lx, Ly).
command_bool(Assoc0, AssocN, String) :-
    phrase(turn_on(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(set_on, Assoc0, AssocN, Lx, Ly).
command_bool(Assoc0, AssocN, String) :-
    phrase(turn_off(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(set_off, Assoc0, AssocN, Lx, Ly).

plus(X,Y,S) :- S #= X+Y.

sol06a(N) :- phrase_from_file(lines(CodeLines), '../day06.txt'),
             maplist(string_codes, Strings, CodeLines),
             maplist(string_chars, Strings, Lines),
             empty_assoc(Assoc0),
             foldl_assoc(command_bool, Assoc0, AssocN, Lines),
             assoc_to_values(AssocN, States),
             foldl(plus, States, 0, N).

% problem b
plus_nat(A,B,S) :- S #= max(0, A+B).

add_toggle(Assoc0, Assoc1, X, Y) :-
    ( get_assoc(X-Y, Assoc0, V0) ; V0 #= 0), 
    V1 #= V0 + 2,
    put_assoc(X-Y, Assoc0, V1, Assoc1). 

add_on(Assoc0, Assoc1, X, Y) :-
    ( get_assoc(X-Y, Assoc0, V0) ; V0 #= 0), 
    V1 #= V0 + 1,
    put_assoc(X-Y, Assoc0, V1, Assoc1).

add_off(Assoc0, Assoc1, X, Y) :-
    ( get_assoc(X-Y, Assoc0, V0) ; V0 #= 0), 
    plus_nat(V0, -1, V1),
    put_assoc(X-Y, Assoc0, V1, Assoc1).

% dispatch
command_int(Assoc0, AssocN, String) :-
    phrase(toggle(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(add_toggle, Assoc0, AssocN, Lx, Ly).
command_int(Assoc0, AssocN, String) :-
    phrase(turn_on(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(add_on, Assoc0, AssocN, Lx, Ly).
command_int(Assoc0, AssocN, String) :-
    phrase(turn_off(X0,Y0,X1,Y1), String), !,
    numlist(X0, X1, Lx), numlist(Y0, Y1, Ly),
    foldl_rect(add_off, Assoc0, AssocN, Lx, Ly).

sol06b(N) :- phrase_from_file(lines(CodeLines), '../day06.txt'),
             maplist(string_codes, Strings, CodeLines),
             maplist(string_chars, Strings, Lines),
             empty_assoc(Assoc0),
             foldl_assoc(command_int, Assoc0, AssocN, Lines),
             assoc_to_values(AssocN, States),
             foldl(plus, States, 0, N).