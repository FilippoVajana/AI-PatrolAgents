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

%%	NOTA: I seguenti assert servono solo a testare estrai_passi
:-assert(conosce(step_ronda(s1,p(0,1),su,0))).
:-assert(conosce(step_ronda(s1,p(0,2),su,1))).
:-assert(conosce(step_ronda(s1,p(0,3),su,2))).
:-assert(conosce(step_ronda(s1,p(0,3),destra,3))).
:-assert(conosce(step_ronda(s1,p(0,3),giu,4))).
:-assert(conosce(step_ronda(s1,p(0,2),giu,5))).
:-assert(conosce(step_ronda(s1,p(0,1),giu,6))).

estrai_passi(S,Ora,Ora,Durata,[step_ronda(S,P,D,T)]):-
	conosce(step_ronda(S,P,D,Ora)),
	T is Ora + Durata.
estrai_passi(S,UltimaVolta,Ora,Durata,[step_ronda(S,P,D,T) | Coda]) :-
	UltimaVolta =< Ora,
	conosce(step_ronda(S,P,D,UltimaVolta)),
	T is UltimaVolta + Durata,
	Next is UltimaVolta + 1,
	estrai_passi(S,Next,Ora,Durata,Coda).
%%	TESTATO, FUNZIONA. Query: estrai_passi(s1,0,6,7,L).

direzione_opposta(n,s).
direzione_opposta(s,n).
direzione_opposta(e,o).
direzione_opposta(o,e).

passo_avanti(PStart,D,PEnd) :-
	direzioni(D,DVett),
	next(PStart,DVett,PEnd,_).

percorso_rettilineo(S,P,D,T,Durata,ListaFinale) :-
	linea_retta(S,P,D,T,Durata,ListaAndata),
	last(ListaAndata,step_ronda(S,P1,D1,T1)),
	direzione_opposta(D1,Dopp),
	linea_retta(S,P1,Dopp,T1,Durata,ListaRitorno),
	append(ListaAndata,ListaRitorno,ListaFinale).
%%	TESTATO, FUNZIONA

linea_retta(S,Pos,Dir,TStart,Lun,List) :-
	linea_retta(S,Pos,Dir,TStart,Lun,0,List).
linea_retta(S,PStart,Dir,TStart,Durata,Calcolati,[step_ronda(S,P,Dir,T)|Coda]) :-
	Calcolati < Durata, !,
	passo_avanti(PStart,Dir,P),
	T is TStart + Calcolati,
	NewCalcolati is Calcolati + 1,
	linea_retta(S,P,Dir,TStart,Durata,NewCalcolati,Coda).
linea_retta(_,_,_,_,C,C,[]).
%%	TESTATO, FUNZIONA
