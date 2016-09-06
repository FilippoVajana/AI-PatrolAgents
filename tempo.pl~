:- module('tempo',[clock/1,
		   aggiorna_clock/0,
		   azzera_clock/0]).
:- use_module('lib/is_a').

type [{integer}]: tempo.

pred clock(tempo).
	%predicato dinamico che identifica l'istante temporale della simulazione
:- dynamic clock/1.

pred aggiorna_clock.
	%fa avanzare di uno step il tempo della simulazione

pred azzera_clock.
	%porta a 0 il tempo della simulazione

clock(0).

aggiorna_clock :-
	clock(Time),
	New_Time is (Time + 1),
	retractall(clock(_)),
	assertz(clock(New_Time)).

azzera_clock :-
	retractall(clock(_)),
	assertz(clock(0)).
