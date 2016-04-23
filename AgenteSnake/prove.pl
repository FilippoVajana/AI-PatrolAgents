%% FILE PER PROVARE I PREDICATI

game_area(p(0,0)).
game_area(p(1,0)).
game_area(p(2,0)).
game_area(p(3,0)).
game_area(p(5,0)).
game_area(p(6,0)).
game_area(p(7,0)).
game_area(p(8,0)).
game_area(p(9,0)).
game_area(p(0,1)).
game_area(p(1,1)).
game_area(p(2,1)).
game_area(p(3,1)).
game_area(p(4,1)).
game_area(p(5,1)).
game_area(p(6,1)).
game_area(p(0,2)).
game_area(p(1,2)).
game_area(p(2,2)).
game_area(p(3,2)).
game_area(p(5,2)).
game_area(p(6,2)).
game_area(p(7,2)).
game_area(p(8,2)).

print_game_area :-
	bagof(P,game_area(P),List),
	writeln(List).


elimina_non_validi([],[]) :- !.
elimina_non_validi([Testa | Coda],[Testa | NuovaLista]) :-
	game_area(Testa), !,
	elimina_non_validi(Coda,NuovaLista).
elimina_non_validi([_Testa | Coda],NuovaLista) :-
	elimina_non_validi(Coda,NuovaLista).
%%	TESTATO, FUNZIONA


converti_lista([],_,[]) :- !.
converti_lista([S | Coda],P,[st(S,P) | StatiCoda]) :-
	converti_lista(Coda,P,StatiCoda).
%%	TESTATO, FUNZIONA




