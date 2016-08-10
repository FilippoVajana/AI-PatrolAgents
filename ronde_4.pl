:-dynamic(stato_corrente/2).
ronda(r1,p(3,5),o,0):- !.
ronda(r1,p(3,5),o,1):- !.
ronda(r1,p(3,5),o,2):- !.
ronda(r1,p(3,5),o,3):-
	!, retractall(stato_corrente(_,_)),
	random_between(0,1,0)->
	assert(stato_corrente(r1,q1))
	;
	assert(stato_corrente(r1,q2)).

ronda(r1,p(3,5),o,4):-
	stato_corrente(r1,q1), !.
ronda(r1,p(3,5),o,5):-
	stato_corrente(r1,q1), !.
ronda(r1,p(3,5),o,6):-
	stato_corrente(r1,q1), !.

ronda(r1,p(3,5),n,4):-
	stato_corrente(r1,q2), !.
ronda(r1,p(3,5),n,5):-
	stato_corrente(r1,q2), !.
ronda(r1,p(3,5),n,6):-
	stato_corrente(r1,q2), !.

ronda(r1,p(I,J),D,K):-
	K > 6,
	H is K mod 7,
	ronda(r1,p(I,J),D,H).


