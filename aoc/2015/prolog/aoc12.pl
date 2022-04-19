:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

% for b
:- use_module(library(http/json)).

:- set_prolog_flag(double_quotes,chars).

seq([])     --> [].
seq([S|Ss]) --> [S], seq(Ss).

... --> [] | [_], ... .

int(I) --> "-", integer(Ip), {I #= -1*Ip} | integer(I).

ints([I|Is]) --> ..., int(I), !, ints(Is).

ints([]) --> ... .

plus(A,B,S) :- S #= A + B.

sol12a(A,Is) :- phrase_from_file(seq(Codes), '../day12.txt'),
                string_codes(Strings, Codes),
                string_chars(Strings, Chars),
                phrase(ints(Is), Chars),
                foldl(plus, Is, 0, A).


jsonfile_dict(Filename, Dict) :-
    open(Filename, read, Stream), json_read_dict(Stream, Dict), close(Stream).


% TODO
sol12b(Dict) :- jsonfile_dict('../day12.txt', Dict),
                is_dict(Dict).

