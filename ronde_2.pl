ronda(r1,p(3,2),e,0) :- !.
ronda(r1,p(3,3),e,1) :- !.
ronda(r1,p(3,4),e,2) :- !.
ronda(r1,p(3,5),e,3) :- !.
ronda(r1,p(3,4),o,4) :- !.
ronda(r1,p(3,3),o,5) :- !.
ronda(r1,p(I,J),D,K) :-
	K > 5, !,
	H is K mod 6,
	ronda(r1,p(I,J),D,H).
