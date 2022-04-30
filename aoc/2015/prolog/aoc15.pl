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

num(N) --> "-", !, number(P), { N#= P * -1 }.
num(N) --> number(N).

% parse line into a record
record(Name, Capacity, Durability, Flavor, Texture, Calories) -->
    string(Ns), ": capacity ", !, num(Capacity),
    ", durability ", num(Durability),
    ", flavor ", num(Flavor),
    ", texture ", num(Texture),
    ", calories ", num(Calories),
    { atom_to_chars(Na,Ns), downcase_atom(Na,Name) }.

line_term(String, ingredient(Name, Capacity, Durability, Flavor, Texture, Calories)) :-
    phrase(record(Name, Capacity, Durability, Flavor, Texture, Calories), String).

ing_Ing(ingredient(Ing,  _,  _,  _,  _,  _), Ing).
ing_Cap(ingredient(  _,Cap,  _,  _,  _,  _), Cap).
ing_Dur(ingredient(  _,  _,Dur,  _,  _,  _), Dur).
ing_Fla(ingredient(  _,  _,  _,Fla,  _,  _), Fla).
ing_Tex(ingredient(  _,  _,  _,  _,Tex,  _), Tex).
ing_Cal(ingredient(  _,  _,  _,  _,  _,Cal), Cal).

% multiply and sum for scalar product using foldl
sp(X, Y, V0, V1) :- V1 #= V0 + (X * Y).

mul(X,Y,M) :- M #= X*Y.

sum([], 0).
sum([L|Ls], S) :-
    S #= L + Ss,
    sum(Ls, Ss).

score(Ingredients, Amounts, Score, CalSc) :-
    length(Ingredients, Ni), length(Amounts, Na), Ni #= Na, !,
    maplist(ing_Cap, Ingredients, Caps), foldl(sp, Caps, Amounts, 0, CapSc), CapScore #= max(CapSc, 0),
    maplist(ing_Dur, Ingredients, Durs), foldl(sp, Durs, Amounts, 0, DurSc), DurScore #= max(DurSc, 0),
    maplist(ing_Fla, Ingredients, Flas), foldl(sp, Flas, Amounts, 0, FlaSc), FlaScore #= max(FlaSc, 0),
    maplist(ing_Tex, Ingredients, Texs), foldl(sp, Texs, Amounts, 0, TexSc), TexScore #= max(TexSc, 0),
    maplist(ing_Cal, Ingredients, Cals), foldl(sp, Cals, Amounts, 0, CalSc),
    foldl(mul, [CapScore, DurScore, FlaScore, TexScore], 1, Score).

% :-  [A,B,C,D] ins 0..100, sol15a(S,[A,B,C,D]),labeling([max(S)], [A,B,C,D,S]).
sol15a(Score,Amounts) :-
    phrase_from_file(lines(CodeLines), '../day15.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, Ing),
    score(Ing, Amounts, Score, _),
    sum(Amounts, 100).

% :-  [A,B,C,D] ins 0..100, sol15b(S,[A,B,C,D]),labeling([max(S)], [A,B,C,D,S]).
sol15b(Score,Amounts) :-
    phrase_from_file(lines(CodeLines), '../day15.txt'),
    maplist(string_codes, Strings, CodeLines),
    maplist(string_chars, Strings, Lines),
    maplist(line_term, Lines, Ing),
    score(Ing, Amounts, Score, 500),
    sum(Amounts, 100).
