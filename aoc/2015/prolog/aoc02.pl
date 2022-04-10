:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(arithmetic)).
:- use_module(library(lists)).

wh --> [].
wh --> [C], wh, { char_type(C, whitespace)}.

digit(D)       --> [D], { char_type(D, decimal_digit) }.
number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

plus(X,Y,S) :- S #= X+Y.

present_length(L) --> number(Lc), "x", { number_chars(L, Lc) }.
present_width(W)  --> number(Wc), "x", { number_chars(W, Wc) }.
present_height(H) --> number(Hc),      { number_chars(H, Hc) }.

present(L,W,H) --> present_length(L), present_width(W), present_height(H).

presents3([],[],[])             --> [].
presents3([L],[W],[H])          --> present(L,W,H).
presents3([L|Ls],[W|Ws],[H|Hs]) --> present(L,W,H), wh, presents3(Ls,Ws,Hs).

smallest_face_area(L,W,H,SFA) :- SFA #= min(L*W,min(W*H,H*L)).
wrap_area(L,W,H,A) :- smallest_face_area(L,W,H,SFA), A #= 2*(L*W + W*H + H*L) + SFA.

full_wrap_area(Ls,Ws,Hs,A) :- maplist(wrap_area, Ls,Ws,Hs, As), foldl(plus, As, 0, A).

% wrap_area(2,3,4,58).
% wrap_area(1,1,10,43).

sol02a(A) :- phrase_from_file(presents3(Ls,Ws,Hs), '../day02.txt'),
             full_wrap_area(Ls,Ws,Hs,A).
           
volume(L,W,H, Vol) :- Vol #= L*W*H. 
smallest_face_perimeter(L,W,H,Per) :- Per #= 2*min(L+W, min(W+H,L+H)).

ribbon_length(L,W,H,Rlen) :-
    volume(L,W,H,Vol),
    smallest_face_perimeter(L,W,H,Per),
    Rlen #= Vol+Per.

full_ribbon_length(Ls,Ws,Hs,Rlen) :- maplist(ribbon_length, Ls,Ws,Hs, Rlens), foldl(plus, Rlens, 0, Rlen).

% very slow, but finally terminates.
sol02b(A) :- phrase_from_file(presents3(Ls,Ws,Hs), '../day02.txt'),
             full_ribbon_length(Ls,Ws,Hs,A).
