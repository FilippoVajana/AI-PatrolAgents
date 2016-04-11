:- dynamic(assoc/1).

build_assoc(A,B, K) :-
	(   K =< 0,
	    A=B
	    ;
	    H is K-1,
	    R is random(1000),
	    put_assoc(k(R), A, '*', C),
	    build_assoc(C,B,H)
	).

build_assoc(K) :-
	retractall(assoc(_)),
	empty_assoc(E),
	build_assoc(E,A,K),
	assert(assoc(A)).
