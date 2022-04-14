:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- set_prolog_flag(double_quotes,chars).

:- dynamic edge/3.

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( call(eol) | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

% parse line into a record
record(From,To,Dist) --> string(Fs), " to ", string(Ts), " = ", number(Dist), !,
                         { atom_to_chars(Fa,Fs), downcase_atom(Fa,From),
                           atom_to_chars(Ta,Ts), downcase_atom(Ta,To)}.

line_term(String, edge(From,To,Dist)) :- phrase(record(From,To,Dist), String).

% edge predicates
edge_from(edge(F,_,_),F).
edge_to(  edge(_,T,_),T).
edge_dist(edge(_,_,D),D).
edge_inv( edge(F,T,D), edge(T,F,D)).

edges_locations(Edges, Locs) :-
    maplist(edge_from, Edges, Froms),
    maplist(edge_to,   Edges, Tos),
    append(Froms, Tos, FromTos),
    list_to_set(FromTos, Locs).

% the TSP DCG
paths(_, []) --> [].
paths([], Locs0) --> 
    [from_to(From,To)],
    {
        select(From, Locs0, Locs1),
        edge(From, To, _),
        select(To, Locs1, Locs2)
    },
    paths([To,From], Locs2).

paths([From|RevPlaces], Locs0) --> 
    [from_to(From,To)],
    {
        edge(From, To, _),
        select(To, Locs0, Locs1)
    },
    paths([To,From|RevPlaces], Locs1).

% post computing the path distance
plus(A,B,S) :- S #= A+B.
from_to_dist(from_to(From,To), Dist) :- edge(From,To,Dist).
path_dist(Path, Dist) :- maplist(from_to_dist, Path, Dists),
                         foldl(plus, Dists, 0, Dist).

% TSP describing all solutions given the input file
sol09(Path, Dist) :- phrase_from_file(lines(CodeLines), '../day09.txt'),
                     maplist(string_codes, Strings, CodeLines),
                     maplist(string_chars, Strings, Lines),
                     maplist(line_term, Lines, Edges),
                     maplist(edge_inv, Edges, InvEdges),
                     retractall(edge(_,_,_)),
                     maplist(assertz, Edges),
                     maplist(assertz, InvEdges),
                     edges_locations(Edges, Locs),
                     phrase(paths([], Locs), Path),
                     path_dist(Path, Dist).

% Solution for part a:
% :- length(_,Dist), sol09(Path,Dist).

% Solution for part b:
% :- length(_,Diff), Dist #= 1100 - Diff, sol09(Path,Dist).
