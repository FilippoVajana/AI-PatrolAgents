:- discontiguous(ignored(_)).
:- use_module(library(is_a)).
:- use_module(livello_spec).

type T :- livello_spec:type(T).
pred P :- livello_spec:pred(P).

direzioni(n,d(-1,0)).
direzioni(s,d(1,0)).
direzioni(e,d(0,-1)).
direzioni(o,d(0,1)).
direzioni(ne,d(-1,-1)).
direzioni(no,d(-1,1)).
direzioni(se,d(1,-1)).
direzioni(so,d(1,1)).

cardinali_opposti(n,s) :- !.
cardinali_opposti(e,o) :- !.
cardinali_opposti(C1,C2) :-
	cardinali_opposti(C2,C1).
direzione_opposta(D1,D2) :-
	direzioni(P1,D1),
	direzioni(P2,D2),
	cardinali_opposti(P1,P2).


next_dirs(d(V,0), d(V,-1), d(V,1)) :- !.
next_dirs(d(0,O), d(-1,O), d(1,O)) :- !.
next_dirs(d(V,O), d(V,0), d(0,O)) :- !.


next(p(X1,Y1), d(V,O), p(X2, Y2), L) :-
	(ground(d(V,O)) -> true; direzioni(_,d(V,O))),
	X2 is X1+V,
	Y2 is Y1+O,
	(
	(V=0; O=0) -> L=1
	; L is sqrt(2)
	).

next(P1,P2) :-
	next(P1,_,P2,_).

adiacenti(P, V) :-
	setof(N, next(P,N), V).

verso(p(X1,Y1), p(X2,Y2), d(S1,S2)) :-
	sign(X2-X1,S1),
	sign(Y2-Y1,S2).

distanza_euclidea(p(X1,Y1), p(X2,Y2), D) :-
		  D is sqrt((X1-X2)^2 + (Y1-Y2)^2).
distanza_quadretti(p(X1,Y1), p(X2,Y2), D) :-
	D is max(abs(X1-X2), abs(Y1-Y2)) - min(abs(X1-X2), abs(Y1-Y2))
	     + sqrt(2)*min(abs(X1-X2), abs(Y1-Y2)).

punto_area(p(I,J),area(p(I0,J0),p(I1,J1))) :-
	between(I0,I1,I),
	between(J1,J0,J).

%% PREDICATI RELATIVI A SENTINELLA  %%

area_sentinella(p(I,J), area(p(X1,Y1),p(X2,Y2))) :-
	X1 is I,
	Y1 is J + 1,
	X2 is I + 4,
	Y2 is J - 1.

punto_mappa(p(I,J),size(R,C)) :-
	Rmax is R-1,
	Cmax is C-1,
	between(0,Rmax,I),
	between(0,Cmax,J).

qualita(' ', 1).
qualita(p, 2).
qualita(o, 999999999).  %sarebbe +infinito


:- dynamic(
	 [map/2,
	  map_size/1,
	  position/1,
	  goal/1
	  ]).

%%	POSIZIONE DEL PRIGIONIERO
pred prigioniero(punto).
%%	pigioniero(?P) SEMIDET
%%	Spec: vero sse P e' la posizione del prigioniero

soldato(P) :-
	position(P).
prigioniero(P) :-
	goal(P).

game_area(P) :-
	map(P,' ').



mostra_mappa(Map) :-
	map_size(size(Rows,Columns)),
	mostra_mappa(Map, size(Rows,Columns)).

mostra_mappa(Map, size(R,C)) :-
	Rows is R-1,
	Columns is C-1,
	forall(between(0,Rows,I),
	       (   forall(between(0,Columns,J), (
			      call(Map,p(I,J),E) -> write(E); write('-'))),
		   nl
	       )).


/**********************  ALCUNI AMBIENTI CARICABILI *************
                         DA UNA LISTA DI ATOMI
 ****************************************************************/


carica_mappa(N) :-
	retractall(map(_,_)),
	retractall(position(_)),
	retractall(goal(_)),
	retractall(map_size(_)),
	ambiente(N, A),
	maplist(atom_chars, A, AA),
	length(A,NR),
	nth0(0,AA,R0),
	length(R0,NC),
	assert(map_size(size(NR,NC))),
	forall(nth0(I,AA,Row),
	       forall(nth0(J,Row,Ch), store_map(p(I,J),Ch))).

store_map(P,x) :- !,
	assert(map(P,' ')),
	assert(position(P)).
store_map(P,g) :- !,
	assert(map(P,' ')),
	assert(goal(P)).
store_map(P, Ch) :-
	assert(map(P,Ch)).

ignored(ambiente(_,_)).

ambiente(1, [
'oooooooooo',
'o   x    o',
'o   ooo  o',
'o  oo    o',
'o  oo oo o',
'o    ooooo',
'o      g o',
'oooooooooo']).

ambiente(2, [
'oooooooooo',
'o   x    o',
'o   ooo  o',
'o  oo    o',
'o  oo oo o',
'ooo oooooo',
'o      g o',
'oooooooooo']).

ambiente(3, [
'oooooooooo',
'o   x    o',
'o   ooo  o',
'o  oo    o',
'o  oo oo o',
'oooooooooo',
'o      g o',
'oooooooooo']).












