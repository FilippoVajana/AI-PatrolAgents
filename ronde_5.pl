ronda(r1,p(3,4),o,0):- !.
ronda(r1,p(3,4),o,1):- !.
ronda(r1,p(3,4),o,2):- !.
ronda(r1,p(3,4),n,3):- !.
ronda(r1,p(3,4),n,4):- !.
ronda(r1,p(3,4),n,5):- !.
ronda(r1,p(3,4),n,6):- !.
ronda(r1,p(I,J),D,K):-
	K > 6,
	H is K mod 7,
	ronda(r1,p(I,J),D,H).
