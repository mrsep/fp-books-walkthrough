:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4, del_assoc/4, get_assoc/3, assoc_to_values/2]).
:- use_module(library(between), [numlist/3, between/3]).
:- use_module(library(format)).

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

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).

% parse number
digit(D)       --> [D], { char_type(D, decimal_digit) }.
number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

% parse rectangular interval
corner(X,Y)       --> number(Xc), ",", number(Yc), {number_chars(X, Xc), number_chars(Y, Yc)}.
rect(X0,Y0,X1,Y1) --> corner(X0,Y0), " through ", corner(X1,Y1), { X0 #=< X1, Y0 #=< Y1 }.

% parse commands
toggle(X0,Y0,X1,Y1) --> "toggle ", rect(X0,Y0,X1,Y1).
turn_on(X0,Y0,X1,Y1) --> "turn on ", rect(X0,Y0,X1,Y1).
turn_off(X0,Y0,X1,Y1) --> "turn off ", rect(X0,Y0,X1,Y1).

non(true, false).
non(false, true).

bool_int(true, 1) :- !.
bool_int(false, 0).

eval([], false) :- !.
eval([on|_], true) :- !.
eval([off|_], false) :- !.
eval([toggle,off|_], true) :- !.
eval([toggle, on|_], false) :- !.
eval([toggle, toggle|Ls], State) :- !, eval(Ls, State).
eval([toggle|Ls], State1) :- non(State1, State0), eval(Ls, State0).

string_cmd(String, cmd(toggle,X0,Y0,X1,Y1)) :- phrase(toggle(X0,Y0,X1,Y1), String), !.
string_cmd(String, cmd(    on,X0,Y0,X1,Y1)) :- phrase(turn_on(X0,Y0,X1,Y1), String), !.
string_cmd(String, cmd(   off,X0,Y0,X1,Y1)) :- phrase(turn_off(X0,Y0,X1,Y1), String), !.

in_rect(X,Y, cmd(_, X0,Y0,X1,Y1)) :- between(X0, X1, X), between(Y0, Y1, Y).
simplify(cmd(CMD, _,_,_,_), CMD).

coord_cmds(X, Y, Cmds, SimpleCmds) :-
    include(in_rect(X,Y), Cmds, FilteredCmds),
    maplist(simplify, FilteredCmds, SimpleCmds).

eval_coord(X,Y, Cmds, Value) :-
    coord_cmds(X,Y, Cmds, SimpleCmds),
    eval(SimpleCmds, State),
    %format("~d,~d: ~a ", [X,Y,State]), write(SimpleCmds), format("~n",[]),
    bool_int(State, Value).

foldl_rect(V0, VN, Cmds, Lx, Ly) :-
    foldl_rect_(V0, VN, Cmds, Lx, Lx, Ly).

foldl_rect_(VN, VN, _, _, _, []) :- !.
foldl_rect_(V0, VN, Cmds, OLx, [], [Y|Ly]) :-
    format("~d ",[Y]),
    foldl_rect_(V0, VN, Cmds, OLx, OLx, Ly).
foldl_rect_(V0, VN, Cmds, OLx, [X|Lx], [Y|Ly]) :-
    eval_coord(X, Y, Cmds, Value),
    V1 #= V0 + Value,
    foldl_rect_(V1, VN, Cmds, OLx, Lx, [Y|Ly]).

sol06a(N) :- phrase_from_file(lines(Lines), '../day06.txt'),
             maplist(string_cmd, Lines, Cmds),
             reverse(Cmds, RevCmds),
             numlist(0, 999, Lx), numlist(0, 999, Ly),
             foldl_rect(0, N, RevCmds, Lx, Ly).

