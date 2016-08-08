ronda(r1,p(3,1),e,0) :- !.
ronda(r1,p(3,2),e,1) :- !.
ronda(r1,p(3,3),e,2) :- !.
ronda(r1,p(3,4),e,3) :- !.
ronda(r1,p(3,3),o,4) :- !.
ronda(r1,p(3,2),o,5) :- !.
ronda(r1,p(I,J),D,K) :-
	K > 5, !,
	H is K mod 6,
	ronda(r1,p(I,J),D,H).
