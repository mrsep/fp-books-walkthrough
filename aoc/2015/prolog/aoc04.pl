:- use_module(library(clpfd)).

% SWI
:- use_module(library(md5)).
:- set_prolog_flag(double_quotes, chars).
% the double_quotes flag set to chars implies that
% single quotes are strings

% ?- md5_hash('abcdef609043',   '000001dbbfa3a5c83a2d506429c7b00e', []).
% ?- md5_hash('pqrstuv1048970', '000006136ef2ff3b291c85725f17325c', []).

gen_work(PrefixStr, Nonce, Work) :-
    number_string(Nonce, NonceStr),
    %string_chars(PrefixStr, Prefix),
    % required if input string is given in "..."
    string_concat(PrefixStr, NonceStr, Work).

repeat(_, 0, []).
repeat(Char, N, [Char|Ls]) :-
    N #> 0, N1 #= N-1,
    repeat(Char, N1, Ls). 

gen_difficulty_prefix(Difficulty, Prefix) :-
    Difficulty #> 0,
    repeat('0', Difficulty, Ls),
    string_chars(Prefix, Ls).

check_work(Hash, Difficulty) :-
    gen_difficulty_prefix(Difficulty, Prefix),
    string_concat(Prefix, _, Hash).

proof_of_work(Input, Difficulty, Proof) :-
    length(_, Proof),
    gen_work(Input, Proof, Work),
    md5_hash(Work, Hash, []),
    check_work(Hash, Difficulty).


sol04a(Proof) :-
    Input = 'bgvyzdsv',
    Difficulty #= 5,
    proof_of_work(Input, Difficulty, Proof).

sol04b(Proof) :-
    Input = 'bgvyzdsv',
    Difficulty #= 6,
    proof_of_work(Input, Difficulty, Proof).

