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

% parse rectangular interval
corner(X,Y)       --> number(X), ",", number(Y).
rect(X0,Y0,X1,Y1) --> corner(X0,Y0), " through ", corner(X1,Y1), { X0 #=< X1, Y0 #=< Y1 }.

% parse commands
toggle(X0,Y0,X1,Y1) --> "toggle ", rect(X0,Y0,X1,Y1).
turn_on(X0,Y0,X1,Y1) --> "turn on ", rect(X0,Y0,X1,Y1).
turn_off(X0,Y0,X1,Y1) --> "turn off ", rect(X0,Y0,X1,Y1).

non(true, false).
non(false, true).

bool_int(true, 1).
bool_int(false, 0).

eval_bool([], false).
eval_bool([on|_], true).
eval_bool([off|_], false).
eval_bool([toggle,off|_], true) :- !.
eval_bool([toggle, on|_], false) :- !.
eval_bool([toggle, toggle|Ls], State) :- !, eval_bool(Ls, State).
eval_bool([toggle|Ls], State1) :- non(State1, State0), eval_bool(Ls, State0).

string_cmd(String, cmd(toggle,X0,Y0,X1,Y1)) :- phrase(toggle(X0,Y0,X1,Y1), String).
string_cmd(String, cmd(    on,X0,Y0,X1,Y1)) :- phrase(turn_on(X0,Y0,X1,Y1), String).
string_cmd(String, cmd(   off,X0,Y0,X1,Y1)) :- phrase(turn_off(X0,Y0,X1,Y1), String).

in_rect(X,Y, cmd(_, X0,Y0,X1,Y1)) :- between(X0, X1, X), between(Y0, Y1, Y).
simplify(cmd(CMD, _,_,_,_), CMD).

coord_cmds(X, Y, Cmds, SimpleCmds) :-
    include(in_rect(X,Y), Cmds, FilteredCmds),
    maplist(simplify, FilteredCmds, SimpleCmds).

eval_bool_coord(X,Y, Cmds, Value) :-
    coord_cmds(X,Y, Cmds, SimpleCmds),
    eval_bool(SimpleCmds, State),
    %format("~d,~d: ~a ", [X,Y,State]), write(SimpleCmds), format("~n",[]),
    bool_int(State, Value).

foldl_rect(Goal, V0, VN, Cmds, Lx, Ly) :-
    foldl_rect_(Goal, V0, VN, Cmds, Lx, Lx, Ly).

foldl_rect_(_, VN, VN, _, _, _, []) :- !.
foldl_rect_(Goal, V0, VN, Cmds, OLx, [], [_|Ly]) :- !,
    foldl_rect_(Goal, V0, VN, Cmds, OLx, OLx, Ly).
foldl_rect_(Goal, V0, VN, Cmds, OLx, [X|Lx], [Y|Ly]) :-
    call(Goal, X, Y, Cmds, Value),
    V1 #= V0 + Value,
    foldl_rect_(Goal, V1, VN, Cmds, OLx, Lx, [Y|Ly]).

sol06a(N) :- phrase_from_file(lines(CodeLines), '../day06.txt'),
             maplist(string_codes, Strings, CodeLines),
             maplist(string_chars, Strings, Lines),
             maplist(string_cmd, Lines, Cmds),
             reverse(Cmds, RevCmds),
             numlist(0, 999, Lx), numlist(0, 999, Ly),
             foldl_rect(eval_bool_coord, 0, N, RevCmds, Lx, Ly), !.

eval_int(on, 1).
eval_int(off, -1).
eval_int(toggle, 2).

plus_nat(A,B,S) :- S #= max(0, A+B).

eval_int_coord(X,Y, Cmds, Brightness) :-
    coord_cmds(X,Y, Cmds, SimpleCmds),
    maplist(eval_int, SimpleCmds, BrightnessChanges),
    foldl(plus_nat, BrightnessChanges, 0, Brightness).

sol06b(TotalBrightness) :- phrase_from_file(lines(CodeLines), '../day06.txt'),
                           maplist(string_codes, Strings, CodeLines),
                           maplist(string_chars, Strings, Lines),
                           maplist(string_cmd, Lines, Cmds),
                           numlist(0, 999, Lx), numlist(0, 999, Ly),
                           foldl_rect(eval_int_coord, 0, TotalBrightness, Cmds, Lx, Ly), !.

