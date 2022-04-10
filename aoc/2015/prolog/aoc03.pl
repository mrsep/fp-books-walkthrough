:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).

move(coord(X,Y), '>', coord(X1,Y)) :- X1 #= X+1.
move(coord(X,Y), '<', coord(X1,Y)) :- X1 #= X-1.
move(coord(X,Y), '^', coord(X,Y1)) :- Y1 #= Y+1.
move(coord(X,Y), 'v', coord(X,Y1)) :- Y1 #= Y-1.

move_grid([], _, AssocN, AssocN).
move_grid([Dir|Dirs], coord(X0,Y0), Assoc0, AssocN) :-
    move(coord(X0,Y0), Dir, coord(X1,Y1)),
    (get_assoc(coord(X1,Y1), Assoc0, V0); V0 #= 0),
    V1 #= V0 + 1,
    put_assoc(coord(X1,Y1), Assoc0, V1, Assoc1),
    move_grid(Dirs, coord(X1,Y1), Assoc1, AssocN).

sol03a(A) :- phrase_from_file(seq(Dirs), '../day03.txt'),
             empty_assoc(Assoc),
             move_grid(Dirs, coord(0,0), Assoc, AssocN),
             assoc_to_values(AssocN, Visits),
             length(Visits, A).

move_grid2([], _, _, AssocN, AssocN).
move_grid2([Dir], coord(X0,Y0), _, Assoc0, AssocN) :- 
    move_grid([Dir], coord(X0,Y0), Assoc0, AssocN).
move_grid2([DirXY,DirAB|Dirs],
           coord(X0,Y0), coord(A0,B0),
           Assoc0, AssocN) :-
    move(coord(X0,Y0), DirXY, coord(X1,Y1)),
    move(coord(A0,B0), DirAB, coord(A1,B1)),
    (get_assoc(coord(X1,Y1), Assoc0, V0); V0 #= 0),
    (get_assoc(coord(A1,B1), Assoc0, W0); W0 #= 0),
    V1 #= V0 + 1, W1 #= W0 + 1,
    put_assoc(coord(X1,Y1), Assoc0, V1, Assoc1),
    put_assoc(coord(A1,B1), Assoc1, W1, Assoc2),
    move_grid2(Dirs, coord(X1,Y1), coord(A1,B1), Assoc2, AssocN).

sol03b(A) :- phrase_from_file(seq(Dirs), '../day03.txt'),
             empty_assoc(Assoc),
             put_assoc(coord(0,0), Assoc, 2, Assoc1),
             move_grid2(Dirs, coord(0,0), coord(0,0), Assoc1, AssocN),
             assoc_to_values(AssocN, Visits),
             length(Visits, A).
