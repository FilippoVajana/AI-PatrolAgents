:- use_module('../is_a').
:- consult(util).

/*  DEDFINISCE: **********************************
             solve/2,
	     solve/3
**************************************************/


type [[ ], [X | list(X)]]:list(X).
type [dl(list(X),list(X))]: difflist(X).
type number.

%%%%   TIPI DEFINITI DALLA STRATEGIA
type frontiera(_).
type potatura(_).

%%    TIPO per la rappresentazione interna dei nodi
%     TNP : Tipo Nodi Problema, definito dal problema
%
type [nc(TNP,list(TNP),number)]:nodo(TNP).
type [trovato]:pred(_). %metatype for call


%%%%%%%% PREDICATI DEFINITI DAL PROBLEMA

pred trovato(_TNP).
   %  trovato(+N) is semidet:    N e' un goal, dipende dal problema
pred vicini(TNP, list(TNP)).
   % vicini(+N, -L) is det:   L e' la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred costo(TNP, TNP, number).
   % costo(+N1,+N2,-C) is det: C e' il costo dell'arco (N1,N2)

pred h(_TNP, number).
   % h(+N,-H) is det:  N e' il nodo corrente, H e' la stima
   % euristica del costo da N ad una soluzione ottimale


%%%%   PREDICATI DEFINITI DALLA FRONTIERA E DALLA STRATEGIA

pred frontiera_iniziale(nodo(TNP),frontiera(nodo(TNP)),potatura(nodo(TNP))).
   % frontiera_iniziale(+N,-F, C) is det:   F e' la frontiera con il solo
   %  nodo N,  C contiene i chiusi e/o altre informazioni di potatura
   %  in pota_chiusi contiene i chiusi, nelle altre strategie implelemtate
   %  non è usato
   %  Inoltre può inizializzare dati globali

pred scelto(nodo(TNP), frontiera(nodo(TNP)), frontiera(nodo(TNP))).
  % scelto(-N, +F0,-F1) is det: N e' un nodo di F0 (il nodo selezionato)
  %			 e F1 e' F0 senza N;
pred aggiunta(list(nodo(TNP)), frontiera(nodo(TNP)), frontiera(nodo(TNP))).
   % aggiunta(+L, +F1, -F2) is det:      F2  si ottiene aggiungendo L ad F1
   % in base alla priorità e algoritmo dati dalla strategia

pred potatura(nodo(TNP), potatura(nodo(TNP)), difflist(nodo(TNP))).
  %  potatura(S, L, C, dl(L1,L2)) det:
  %  L2 aggiunge a L1 il risultato della potatura di S in base a C
  %  - se S non è potato, aggiunge S
  %  - se S è sostituito da un chiuso migliore SC, contiene SC
  %  - se S è potato nulla viene aggiunti
  %  - aperto ad altre strategie di potatura

pred chiusura(nodo(TNP), potatura(nodo(TNP)), potatura(nodo(TNP))).
  %  A) se non si mantiene una lista di nodi chiusi per la potatura,
  %  ha successo senza effetti;
  %  B) altrimenti aggiunge TNP alla base dati dei nodi chiusi; in tal
  %  caso taglia_cicli taglia piu' pesantemente i nodi chiusi



%% PREDICATI DEFINITI DAL PROGRAMMA
%
pred target(_TNP).
pred is_target(_TNP).  %dynamic
  %  target(+G) semidet
  %  Vero sse G è un goal; se si usa solve/2 e trovato/1 è definito dal
  %          problema, target(G) equivale a trovato(G);
  %	     se si usa solve/3 e call(Trovato,G), equivale a
  %          call(Trovato,G); is_target memorizza il parametro Trovato

pred solve(TNP, nodo(TNP)).
  % solve(+Start,nc(-Goal,-Path,-Cost)) is nondet:   da Start si raggiunge
  %    Goal attraverso il cammino Path con costo Cost; Goal e' una
  %    soluzione

pred  solve(TNP, nodo(TNP), pred(TNP)).
  % solve(+Start, nc(-Goal,-Path,-Cost)m, +Trovato) is nondet:
  %   Path cammino da nodo start a Goal  e call(Trovato,Goal) è vero


pred cerca(frontiera(nodo(TNP)),nodo(TNP), pred(TNP), potatura(nodo(TNP))).
  %  cerca(+F,nc(-S,-Path,-Cost), +Trovato, +P) is nondet:
  %  Spec:
  %  -   per ogni goal G vi e' un cammino da un nodo di F a G
  %      inizialmente F=nodo iniziale, poi ricorsivamente altre frontiere
  %  -   Path è il cammino dal nodo iniziale ad S,  vale call(Trovato,S)
  %      e S è la soluzione trovata dalla strategia applicata
  %  -	 P contiene le informazioni di potatura, se usate dalla strategia

pred trasforma(list(TNP),nodo(TNP),potatura(nodo(TNP)), difflist(nodo(TNP))).
   % trasforma(+Vicini,+N,+C,dl([],-F)) is det:   F e' la porzione di
   %        frontiera contenente i Vicini, da aggiungere
   %	    alla frontiera corrente

%********************************************************************
%   USATI PER DEBUG E STATISTICHE, no typecheck

prolog_goal(stats_and_debug(frontiera(TNP), potatura(TNP))).
prolog_goal(init_counters).
prolog_goal(inc_stats(_)).
prolog_goal(end_stats(_)).

%****************************  START  **********************

:- dynamic(is_target/1).
target(X) :-
	is_target(Trovato),!,
	call(Trovato,X)
	;
	trovato(X).

solve(N,G, Trovato) :-
      retractall(is_target(_)),
      assert(is_target(Trovato)),
      init_counters,
      frontiera_iniziale(nc(N,[],0),F0, Chiusi),
      cerca(F0,G, Trovato,  Chiusi).

solve(N,G) :-
      retractall(is_target(_)),
      init_counters,
      frontiera_iniziale(nc(N,[],0),F0, Chiusi),
      cerca(F0,G, trovato,  Chiusi).


/***************************  ALGORITMO GENERICO ******************/

cerca(Frontiera, Goal, Trovato,  Chiusi) :-
       stats_and_debug(Frontiera, Chiusi),
       scelto(nc(PN, Path, Cost),Frontiera,F1), % dalla strategia
       (   call(Trovato, PN),                         % dal problema
	   Goal = nc(PN, Path, Cost)
	   , end_stats(Goal)
       ;
	   vicini(PN,Vicini),		         % dal problema
	   trasforma(Vicini,nc(PN,Path,Cost),
		     Chiusi, dl([],Espansione)),
	   chiusura(nc(PN,Path,Cost),Chiusi,NuoviChiusi), %dalla strategia
	   aggiunta(Espansione,F1,NuovaFrontiera),
	            % dalla strategia
	   inc_stats(Espansione, Path),
	   cerca(NuovaFrontiera,Goal, Trovato, NuoviChiusi)
       ).

trasforma([],nc(_,_,_),_,dl(R,R)).
trasforma([V|T], nc(N,Path,Cost),Chiusi,dl(R1,R2)) :-
	costo(N,V,K),
	Cost1 is Cost+K,
        potatura(nc(V,[N|Path],Cost1), Chiusi,dl(R2,R3)),
        trasforma(T,nc(N,Path,Cost),Chiusi,dl(R1,R3)).




%---------------------- PARTE B ----------------------------/

/******************* STATS and DEBUG **********************/

:- dynamic(depth_stats/4).
:- dynamic(stat_in/0).
:- dynamic(showflag/0).

show :-
	nb_setval(count,0),
	assert(showflag),
	assert(stat_in),
	writeln(
'.... entro in modalita'' interattiva
 con statistiche attive'),
	sh.
noshow :-
	writeln('modalita'' interattiva disattivata'),
	retractall(showflag).

stats_on :-
	stats_off,
	assert(stat_in).
stats_off :-
	retractall(stat_in).

stats_and_debug(F,_C) :-
	do_stats(F),
	(   showflag -> mostra(F),command; true).

init_counters :-
      nb_setval(count,0),
      start_stats.

/********************** GESTIONE CONTATORI STATISTICHE *********/
start_stats :-
	nb_setval(depth,0),
	nb_setval(maxdepth, 0),
	nb_setval(num_nodi,1),
	nb_setval(f_max,1),
	nb_setval(frontiera,1),
	nb_setval(goal, nil),
	nb_setval(bf, undef),
	retractall(depth_stats(_,_,_,_)).

do_stats(_F) :-
	stat_in,!,
	nb_getval(depth, D),
	nb_getval(maxdepth, MD),
	nb_getval(num_nodi, Now),
	(   D > 0 -> BF is exp(log(Now)/D); BF=9999999999),
	nb_setval(bf,BF),
	nb_getval(frontiera, SizeF),
	nb_getval(f_max, SizePrec),
        MaxSizeF is max(SizePrec, SizeF),
	nb_setval(f_max, MaxSizeF),
	assert(depth_stats(lev:D,fringe:SizeF,nodes:Now,bf:BF)),
	%NewD is D+1,
	%nb_setval(depth, NewD),
	MaxD is max(D,MD),
	nb_setval(maxdepth, MaxD).
do_stats(_F).

inc_stats(L,P) :-
	stat_in,!,
	nb_getval(frontiera, NF),
	nb_getval(num_nodi, ND),
	length(L,NL),
	MF is NF+NL-1,
	MD is ND+NL,
	nb_setval(num_nodi, MD),
	nb_setval(frontiera, MF),
	length([_|P],D),
	nb_setval(depth,D).
inc_stats(_,_).

end_stats(Goal) :-
	stat_in,!,
	nb_setval(goal, Goal),
	print_stats,
	nl,
        writeln('?- depth_stats(Liv,DimF, Nodi, BF) per le stat. per livello').
end_stats(_Goal).


print_stats :-
        win_format(
'nodi espansi ^@, profondita'' ^@, frontiera max. ^@, prof. max. ^@, BF ^@',
[prval(num_nodi), prval(depth), prval(frontiera), prval(maxdepth), prval(bf)]).

prval(GV) :-
	nb_getval(GV, Val),
	write(Val).


/********************     INTERFACCIA DEBUGGING   ****************/

sh :- writeln('comandi:	 n: noshow; a: abort; t: trace; h: help').
command :-
	incr(N),
	nl,
	write(N:' '),
	print_stats,
	nl,
	write(' |:'),
	readln(R),
	(
	R = [], !
	;
	R = [h|_], !, sh, command
	;
	R = [n|_], !, noshow
	%;
	%R = [s|_], !, print_stats
	;
	R = [t|_], !, trace
	;
	R = [a|_], !, abort
        ;
	true
	).

incr(N) :-
	nb_getval(count,N),
	M is N+1,
	nb_setval(count,M).







