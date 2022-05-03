:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(library(reif)).

:- set_prolog_flag(double_quotes,chars).

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( call(eol) | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

light(1) --> "#".
light(0) --> ".".

lightsw(0, '.').
lightsw(1, '#').

% parse line into a record
record([]) --> [].
record([Switch | LightList]) --> light(Switch), record(LightList).

line_term(String, LightList) :-
    phrase(record(LightList), String), !.

lightning(Row-Col, Val, Grid0, Grid1) :-
    lightning(Row, Col, Val, Grid0, Grid1).

lightning(Row, Col, Val, Grid0, Grid1) :-
    put_assoc(Row-Col, Grid0, Val, Grid1).
    
set_grid_line(LightList, Row, Grid0, GridN) :-
    length(LightList, NCols),
    numlist(1, NCols, Cols),
    foldl(lightning(Row), Cols, LightList, Grid0, GridN).

set_grid(LightLists, GridN) :-
    length(LightLists, NRows),
    numlist(1, NRows, Rows),
    is_assoc(Grid0),
    foldl(set_grid_line, LightLists, Rows, Grid0, GridN).

between2(MinF-MinS,MaxF-MaxS, F-S) :-
    between(MinF,MaxF,F),
    between(MinS,MaxS,S).

neighborhood(NRows, NCols, X-Y, NBH) :-
    XM1 #= X-1, XP1 #= X+1,
    YM1 #= Y-1, YP1 #= Y+1,
    include(between2(1-1, NRows-NCols),
            [XM1-YM1,X-YM1,XP1-YM1,
             XM1-Y,        XP1-Y  ,
             XM1-YP1,X-YP1,XP1-YP1],
            NBH).

matrix_dims(LL, DimX, DimY) :-
    length(LL, DimX),
    maplist(length, LL, [DimY|DimYs]),
    maplist(=(DimY), DimYs).

read_assoc(Assoc, Key, Value) :- get_assoc(Key, Assoc, Value).

add(X,Y,S) :- S #= X+Y.

neighborhood_score(NRows, NCols, Grid, X-Y, Score) :-
    neighborhood(NRows, NCols, X-Y, NBH),
    maplist(read_assoc(Grid), NBH, NbVals),
    foldl(add, NbVals, 0, Score).

cell_transition(NRows, NCols, Grid, X-Y, 0, Val1) :-
    neighborhood_score(NRows, NCols, Grid, X-Y, Score),
    Score #= 3 -> Val1 #= 1; Val1 #= 0.
cell_transition(NRows, NCols, Grid, X-Y, 1, Val1) :-
    neighborhood_score(NRows, NCols, Grid, X-Y, Score),
    between(2,3,Score) -> Val1 #= 1; Val1 #= 0.

grid_transition(NRows, NCols, Fixed, Grid0, GridN) :-
    assoc_to_keys(Grid0, Cells),
    maplist(read_assoc(Grid0), Cells, Vals0),
    maplist(cell_transition(NRows, NCols, Grid0), Cells, Vals0, Vals1),
    foldl(lightning, Cells, Vals1, Grid0, GridM),
    length(Fixed, NFixed),
    repeat(1, NFixed, Ones),
    foldl(lightning, Fixed, Ones, GridM, GridN).

pair(F,S, F-S).

repeat(_, 0, []).
repeat(Char, N, [Char|Ls]) :-
    N #> 0, N1 #= N-1,
    repeat(Char, N1, Ls). 

iterate(_, 0, ArgN, ArgN) :- !.
iterate(Goal, N, Arg0, ArgN) :-
    call(Goal, Arg0, Arg1),
    N1 #= N-1, !,
    iterate(Goal, N1, Arg1, ArgN).

sol18(Grid, NRows, NCols) :-
    phrase_from_file(lines(CodeLines), '../day18.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, LightLists),
    matrix_dims(LightLists, NRows, NCols),
    set_grid(LightLists, Grid).

print_grid(NRows, NCols, Grid) :-
    numlist(1, NRows, Rows),
    maplist(print_line(Grid, NCols), Rows).

print_line(Grid, NCols, Row) :-
    numlist(1, NCols, Cols),
    maplist(pair(Row), Cols, Cells),
    maplist(read_assoc(Grid), Cells, Vals),
    maplist(lightsw, Vals, Chars),
    string_chars(Chars,S),
    writef(S), writef("\n").

sol18a(Non) :-
    sol18(Grid, NRows, NCols),
    %print_grid(NRows, NCols, Grid),
    iterate(grid_transition(NRows, NCols, []), 100, Grid, GridN),
    %print_grid(NRows, NCols, GridN),
    assoc_to_values(GridN, Vals),
    foldl(add, Vals, 0, Non).

sol18b(Non) :-
    sol18(Grid, NRows, NCols),
    Fixed = [1-1,1-NCols, NRows-1, NRows-NCols],
    iterate(grid_transition(NRows, NCols, Fixed), 100, Grid, GridN),
    assoc_to_values(GridN, Vals),
    foldl(add, Vals, 0, Non).

