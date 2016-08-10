:- dynamic(stato_corrente/2).
ronda(r1,p(2,5),s,0):- !.
ronda(r1,p(3,5),s,1):- !.
ronda(r1,p(4,5),s,2):- !,
	% punto della ronda in cui avviene la biforcazione non deterministica
	retractall(stato_corrente(r1,_)),
	random_between(0,1,0) ->
	assert(stato_corrente(r1,q1))
	;
	assert(stato_corrente(r1,q2)).

%%	Q1
ronda(r1,p(4,5),s,3):-
	stato_corrente(r1,q1), !.
ronda(r1,p(4,5),s,4):-
	stato_corrente(r1,q1), !.
ronda(r1,p(4,5),s,5):-
	stato_corrente(r1,q1), !.
ronda(r1,p(4,5),s,6):-
	stato_corrente(r1,q1), !.
ronda(r1,p(4,5),s,7):-
	stato_corrente(r1,q1), !.

%%	Q2
ronda(r1,p(5,5),s,3):-
	stato_corrente(r1,q2), !.
ronda(r1,p(6,5),s,4):-
	stato_corrente(r1,q2), !.
ronda(r1,p(7,5),s,5):-
	stato_corrente(r1,q2), !.
ronda(r1,p(6,5),n,6):-
	stato_corrente(r1,q2), !.
ronda(r1,p(5,5),n,7):-
	stato_corrente(r1,q2), !.

%%	punto della ronda in cui le biforcazioni si riuniscono
ronda(r1,p(4,5),n,8):- !.
ronda(r1,p(3,5),n,9):- !.
ronda(r1,p(2,5),n,10):- !.

ronda(r1,p(I,J),D,K) :-
	K > 10, !,
	H is K mod 11,
	ronda(r1,p(I,J),D,H).
