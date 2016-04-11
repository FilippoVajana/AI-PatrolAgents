:- module(mr, [search_help/0,
	       strategy/1,
	       target/1,
	       solve/2,
	       solve/3,
	       depth_stats/4,
	       show/0,
	       stats_on/0,
	       stats_off/0,
	       noshow/0]
	 ).

ignored(init_counters).
ignored(frontiera_iniziale(_, _, _)).
ignored(cerca(_, _, _, _)).

/******************  INTERFACCIA DIDATTICA ********************************/
/****** NB:  IL PROGETTO DOVRA' FORNIRE LA 'SUA' INTERFACCIA !! ***********/

/********** Schermata iniziale ************************/
:- writeln(
'*****************************************************************************
STRATEGIE RICERCA DISPONIBILI:
    profondita, ampiezza, astar, costo_minimo, best_first,
    [taglio_cicli, <strategia>],
    [pota_chiusi, <strategia>]'
),
writeln('search_help.  per help\n
*****************************************************************************'),
consult('Ricerca/search').


/********************** HELP *************************/
search_help :- maplist(writeln,[
'*****************************************************************************
STRATEGIE RICERCA DISPONIBILI:
    profondita, ampiezza, astar, costo_minimo, best_first,
    [taglio_cicli, <strategia>],
    [pota_chiusi, <strategia>]'
,
'comandi:
    strategy(+S: nome strategia)
          per scegliere la strategia
    solve(+Start:nodo_problema, -Soluzione: nc)
         solve/2 richiede che trovato(G) sia definito dal problema
    solve(+Start:nodo_problema, -Soluzione: nc, +Trovato:pred)
         con solve/3 si passa il predicato Trovato',
'   target(?G:nodo problema)   equivale a trovato(G) se si usa solve/2
			      e a call(G,Trovato) se si usa solve/3',
'    stats_on  [statistiche]
    stats_off',
'    show  [per debugging]
    noshow',
'Problema:  deve definire
    trovato(?nodo).     trovato(N) : N e'' un goal
                        sostituibile con parametro in solve/3
    vicini(+nodo,-list(nodo)).   vicini(N,L) : L lista dei vicini di N
    Se vi e'' un costo non unitario:
       costo(+nodo, +nodo, -number). costo(N1,N2,C) : C costo di N1->N2
    Se la strategia usa un''euristica:
       h(+nodo, -number).   h(N,E) : E valore euristico di N'
,
'*****************************************************************************'
	]).

/*********************  Comandi utente **************************************/

:- dynamic(loaded_strategy/1).
%%	scelta strategia
strategy(S)
:-
retractall(loaded_strategy(_)),
(   is_list(S) -> maplist(s_consult,S); s_consult(default), s_consult(S)),
maplist(write, ['\n*********************       CARICATA STRATEGIA ',S, '       ****************\n\n']).


s_consult(LF) :-
	is_list(LF),!,
	maplist(s_consult, LF).
s_consult(F) :-
	atom_concat('Ricerca/',F, RF),
	load_files(library(RF),[silent(true)]),
	assert(loaded_strategy(F)).

/********************* FINE INTERFACCIA DIDATTICA **************************/

