:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- set_prolog_flag(double_quotes,chars).

:- dynamic edge/3.

% derived from aoc09 TSP
% adjustments for parsing
% support circular TSP
% remove inv edges, since all data is given

% parse file into list of lines
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( call(eol) | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

% parse line into a record
record(From,To,Happ) --> string(Fs), " would lose ", !, number(Sad),
                         " happiness units by sitting next to ", string(Ts), ".",
                         { atom_to_chars(Fa,Fs), downcase_atom(Fa,From),
                           atom_to_chars(Ta,Ts), downcase_atom(Ta,To),
                           Happ #= -1 * Sad }.
record(From,To,Happ) --> string(Fs), " would gain ", !, number(Happ),
                         " happiness units by sitting next to ", string(Ts), ".",
                         { atom_to_chars(Fa,Fs), downcase_atom(Fa,From),
                           atom_to_chars(Ta,Ts), downcase_atom(Ta,To) }.

line_term(String, edge(From,To,Happ)) :- phrase(record(From,To,Happ), String).

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

% TSP path generator
gen_path(Locs, Path, Happ) :- gen_path_(Locs, [], Path, 0, Happ).

gen_path_(   [], Path, Path, Happ, HappN) :-
    % close circle
    % note that Path is build up in reverse order
    [from_to(_,LastTo)|_] = Path,
    reverse(Path, [from_to(FirstFrom,_) | _]),
    edge(FirstFrom, LastTo, HappC0),
    edge(LastTo, FirstFrom, HappC1),
    HappN #= Happ + HappC0 + HappC1.

gen_path_(Locs0,   [], Path,    0, HappN) :-
    edge(From, To, HappF),
    edge(To, From, HappB),
    Happ #= HappF + HappB,
    select(From, Locs0, Locs1),
    select(To, Locs1, Locs2),
    gen_path_(Locs2, [from_to(From,To)], Path, Happ, HappN).

gen_path_(Locs0, [from_to(F,From)|Path0], Path, Happ0, HappN) :-
    edge(From, To, HappF),
    edge(To, From, HappB),
    select(To, Locs0, Locs1),
    Happ1 #= Happ0 + HappF + HappB,
    gen_path_(Locs1, [from_to(From,To),from_to(F,From)|Path0], Path, Happ1, HappN).

% TSP describing all solutions given the input file
sol13(Path, Happ) :- phrase_from_file(lines(CodeLines), '../day13.txt'),
                     maplist(string_codes, Strings, CodeLines),
                     maplist(string_chars, Strings, Lines),
                     maplist(line_term, Lines, Edges),
                     retractall(edge(_,_,_)),
                     maplist(assertz, Edges),
                     edges_locations(Edges, Locs),
                     gen_path(Locs, Path, Happ).

% Solution for part a:
% :- length(_,Happ), Happ #= 700 - Diff, sol13(Path,Happ).
