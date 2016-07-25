:- module('tempo',[clock/1,
		   aggiorna_clock/0,
		   azzera_clock/0]).


:- dynamic(clock/1).
clock(0).

aggiorna_clock :-
	clock(Time),
	New_Time is (Time + 1),
	retractall(clock(_)),
	assertz(clock(New_Time)).

azzera_clock :-
	retractall(clock(_)),
	assertz(clock(0)).
