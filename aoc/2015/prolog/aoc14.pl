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

% parse line into a record
record(Name, Speed, FlyTime, RestTime) -->
    string(Fs), " can fly ", !, number(Speed), " km/s for ", number(FlyTime),
    " seconds, but then must rest for ", number(RestTime), " seconds.",
    { atom_to_chars(Fa,Fs), downcase_atom(Fa,Name) }.

line_term(String, reindeer(Name, Speed, FlyTime, RestTime)) :-
    phrase(record(Name,Speed, FlyTime, RestTime), String).

% reindeer projection predicates
reindeer_cycle(reindeer(_,_,T1,T2),CT) :- CT #= T1 + T2.

reindeer_distance(TargetTime, R, N-Distance) :-
    R = reindeer(N,S,FT,_),
    reindeer_cycle(R, CT),
    NumCycles #= TargetTime // CT,
    LastCycleTime #= TargetTime - NumCycles * CT,
    LastCycleFlyTime #= min(FT, LastCycleTime),
    TotalFlyTime #= NumCycles * FT + LastCycleFlyTime,
    Distance #= S * TotalFlyTime.

ndleq(_-D1, _-D2) :- D1 @=< D2.
fastest_reindeer(NameDist, Name, Dist) :-
    max_member(ndleq, Name-Dist, NameDist).

sol14a(TargetTime, Name, Dist) :-
    phrase_from_file(lines(CodeLines), '../day14.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, Reindeers),
    maplist(reindeer_distance(TargetTime), Reindeers, NameDistance),
    fastest_reindeer(NameDistance, Name, Dist).

list_iterate(Begin, End, Generator, Const, ResultList) :-
    Begin #=< End,
    list_iterate_(Begin, End, Generator, Const, [], ResultList).

list_iterate_(End, End, _, _, ResultList, ResultList).
list_iterate_(Begin, End, Generator, Const, CurList, ResultList) :-
    Begin #< End, !,
    call(Generator, Const, Begin, Result),
    append(Result, CurList, NextList),
    Begin1 #= Begin + 1,
    list_iterate_(Begin1, End, Generator, Const, NextList, ResultList).

first(First, First-_).
second(Second, _-Second).

fastest_reindeers(NameDist, Winners) :-
    max_member(ndleq, _-Dist, NameDist),
    include(second(Dist), NameDist, WinnerDist),
    maplist(first, Winners, WinnerDist).

generate(Reindeers, TargetTime, Names) :-
    maplist(reindeer_distance(TargetTime), Reindeers, NameDistance),
    fastest_reindeers(NameDistance, Names).

sol14b(TargetTime, Winner, NameWins) :-
    phrase_from_file(lines(CodeLines), '../day14.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, Reindeers),
    TT #= TargetTime + 1,
    list_iterate(1, TT, generate, Reindeers, Winners),
    sort(0, @=<, Winners, SortedWinners),
    clumped(SortedWinners, NameWins),
    max_member(ndleq, Winner, NameWins).
