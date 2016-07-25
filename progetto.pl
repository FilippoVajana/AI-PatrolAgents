:- use_module(library(is_a)).
:- discontiguous(type(_)).
:- discontiguous(pred(_)).
:- discontiguous(gen(_)).


:- use_module(library(mr)).
:- use_module(sentinella).
:- use_module(tempo).
:- consult(livello).
:- consult(vai).

/*****  IMPORTO type, gen, pred DAI MODULI DI SPECIFICA ***/
:- use_module(vai_spec).
:- use_module(vai_if).
:- use_module(livello_spec).



%  CARICO L'INERFACCIA UTENTE
:- consult(interfaccia_utente).



type T :- vai_spec:type(T).
type T :- vai_if:type(T).
type T :- livello_spec:type(T).
type T :- sentinella:type(T).
type T :- tempo:type(T).
pred P :- vai_spec:pred(P).
pred P :- vai_if:pred(P).
pred P :- livello_spec:pred(P).
pred P :- sentinella:pred(P).
pred P :- tempo:pred(P).



/***   IMPLEMENTO I TIPI DI vai_if ******/

gen [mappa(number)]:info_inizio.
    % mappa(I):  identifica la mappa caricabile n. I

gen [st(punto, punto, tempo)]:stato.
    % st(P,G,T): l'agente si trova in P nel tempo T e deve andare in G


/******   Le decisioni dell'agente  */
gen [vado(punto, punto),
       % decisione vado(P,G): cerco un piano per andare da P a G
     avanzo(punto,tempo),
       % azione avanzo(P,T): avanzo nel punto indicato e nel tempo T
     termino(evento)
    ]: decisione.

gen [map(punto, terreno)]: assumibile.

/***  A:  inizialiizzazioni  *****/

pred clear_knowledge.
% comando qui introdotto per azzerare la conoscenza, che altrimenti
% resta acquisita ad ogni esecuzione e si accumula

clear_knowledge :-
	retractall(assunto(_)),
	retractall(conosce(_)).

% specificato in vai_if; l'informazione iniziale � qui
% la mappa che vogliamo usare; se gi� caricata non viene
% azzerata conoscenza dinamica dell'agente, che di volta in volta
% impara; se si passa a nuova mappa, l'agente parte con
% conoscenza dinamica nulla;  usiamo  ultima/1 per ricordare
% l'ultima mappa caricata
:- dynamic(ultima/1).

stato_iniziale(st(P0,G,T), mappa(I)) :-
	strategy(astar),
	strategy(pota_chiusi),
	azzera_clock,
	clock(T),
	carica_mappa(I),
	position(P0),
	goal(G),
	(   ultima(I) ->
	    writeln('CONTINUAZIONE SU ':mappa(I))
	;   retractall(ultima(_)),
	    clear_knowledge,
	    writeln('NUOVA ':mappa(I)),
	    assert(ultima(I))
	).


/****  B: le decisioni *******/

% fine � specificato in vai_if; la decisione finale � termino(...)
fine(termino(_)).

% decidi � specificato in vai_if;  lo implemento come segue
% 1) sono a inizio storia con goal G;
decidi(st(P0,G,_T),
       [inizio_storia(_I)],
       % decido di cercare un piano per andare da P0 a G
       vado(P0,G)).

% 2) ho eseguito la decisione precedente _Dec;
decidi(st(DoveSono,G,T),
       [eseguita(avanzo(DoveSono,Tprec))|_],
       Decisione)
:- soldato_avvistato(_,DoveSono,T) ->
   % se sono stato avvistato, termino
   Decisione = termino(impossibile(avanzo(DoveSono,Tprec)))
   ;
   DoveSono = G ->
   % se la mia posizione � il goal, termino
   Decisione = termino(eseguita(avanzo(DoveSono,Tprec)))
   ;
   %  altrimenti cerco un piano per andare da DoveSono a G
   Decisione = vado(DoveSono,G).

% 3) nella ricerca del piano ho verificato l'impossibilita'
% di raggiungere il goal
decidi(_ST,
       [impossibile(vado(P,G))|_],
       % termino tristemente
       termino(impossibile(vado(P,G)))).

/****** C:   LE AZIONI  *******/

% azione � specificato in vai_if;  le azioni sono le seguenti:
azione(avanzo(_,_)).

% esegui_azione � specificato in vai_if;
% la posizione in cui devo andare � visibile; se libera
% ci vado, altrimenti fallisco
esegui_azione(st(P0,G,T0), _Storia, avanzo(P1,T0), st(P1,G,T1)) :-
	aggiorna_clock,
	clock(T1),
	retract(position(P0)),
	assert(position(P1)).

pred libera(punto).
   %  libera(?P) semidet:    P � una posizione della mappa
   %  libera da ostacoli
libera(P) :-
	map(P,T),
	not(T=o),
	foreach(stato_sentinella(_,Pos,_),not(P = Pos)).

/*****  D) La pianificazione delle decisioni non direttamente
 *         eseguibili come azioni                     ********/

% 1) ho deciso di cercare un piano per andare da P a G, lo cerco
piano(_ST, _Storia, vado(P,G), Piano):-
	cerca_un_piano(P, G, Piano).

pred cerca_un_piano(punto,punto, list(decisione)).
%  cerca_un_piano(+P, +G, -Piano) semidet
%  Piano � un piano che porta daP a G, possibile in base alle
%  conoscenze che l'agente ha in questo momento
%  FALLISCE se non ci sono piani, nel qual caso l'agente sa che il goal
%  non � raggiungibile indipendentemente da ci� che ancora non conosce
:- dynamic(current_goal/1).
pred current_goal(punto).
cerca_un_piano(P, G, Piano) :-
	%  RICORDO IL GOAL DI QUESTO PIANO PER USARLO
	%  NELLA EURISTICA che usa la distanza quadretti da G
	retractall(current_goal(_)),
	assert(current_goal(G)),
	%   USO solve imlementato nel modulo ricerca mr.pl
	clock(T),
	solve(passo(P,T), Sol, =(passo(G,_))),
	estrai_piano(Sol,Piano).

type [nc(passo(punto,tempo),list(passo(punto,tempo)),number)]:nodo.
%  non stiamo a importare tutti i tipi di search_spec.pl
%  qui ci basta questo
pred estrai_piano(nodo, list(decisione)).
%  estrai_piano(+Sol, -Piano) det
%  Piano � la sequenza di avanzamenti che percorre la
%  sequenza di posizioni calcolata nella soluzione Sol
estrai_piano(nc(G,RevPath,_C), Piano) :-
	% faccio la reverse perch� il path � dal
	% nodo alla radice e quindi in senso inverso
	reverse([G|RevPath], [_Start|Path]),
	path2moves(Path, Piano).

pred path2moves(list(passo(punto,tempo)), list(decisione)).
path2moves([passo(P,T)|Path],[avanzo(P,Tprec)|MovList]) :-
	Tprec is T - 1,
	path2moves(Path,MovList).
path2moves([],[]).


/*****  D1.  APPLICAZIONE DI A* e del ragionamento basato su
             assunzioni nella ricerca di un piano

	     A* � implementato nel modulo mr.pl e richiede
	     di implementare l'interfaccia search_if.pl

	     Il predicato pensa � implementato in vai.pl

*************************************************************/

%  implemento vicini, richiesto dall'interfaccia search_if
vicini(passo(P,T), V) :-
	T1 is T	+ 1,
	setof(passo(P1,T1), pensa_sicuro(P, P1, T), V),
	!
	;
	V=[].

pred pensa_sicuro(punto,punto,tempo).
%  pensa_sicuro(+P1, -P2) nondet
%  P2 � raggiunibile da P1 ed e' un punto sicuro in base a quanto assume
%  l'agente nello stato di conoscenza attuale
pensa_sicuro(P1,P2,T) :-
	adiacenti(P1,ListaAdiacenti),
	member(P2,ListaAdiacenti),
	libera(P2),
	T1 is T + 1,
	not(pensa_avvistato(_,P2,T1)).

pred pensa_avvistato(id_sentinella,punto,tempo).
%%	pensa_avvistato(?S,?P,?T) SEMIDET
%%	Spec: vero sse l'agente pensa di essere avvistato da S nel punto
%	P al tempo T
pensa_avvistato(S,P,T) :-
	pensa(step_ronda(S,Ps,D,T),_),
	area_sentinella(Ps,D,A),
	punto_area(P,A).

%  implemento il costo, in base a quanto assume l'agente
%  nello stato di conoscenza attuale
costo(passo(P1,_), passo(P2,_), 1) :-
	P1 \== P2.
% implemento l'euristica usando la distanza in quadretti
% implementata in livello.pl; il goal � quello memorizzato
% prima di lanciare la ricerca con solve
h(passo(P,_),H) :-
	current_goal(G),
	distanza_quadretti(P,G,H).




/******************  E)  PARTE DI RAGIONAMENTO  *******************/

%   aggiorna_conoscenza � specificata in vai_if.pl
%   avviene a fronte di un evento verificatosi

%  1) evento inizio_storia(_). All'inizio l'agente si trova in una
%  posizione che, almeno nella prima escuzione, � nuova
aggiorna_conoscenza(st(_P,_G,_), _H, inizio_storia(_Avvio)) :- !,
	%  l'agente si guarda in giro e impara (ricorda) cosa
	%  c'� nel mondo nelle posizioni adiacenti
	clock(T),
	forall(stato_sentinella(S,Psent,D),impara(step_ronda(S,Psent,D,T))).

%   2) evento transizione(S1,A,S2,PL). E' stata eseguita la transizione
%   da S1 a S2, l'agente si trova in una posizione che potrebbe non aver
%   mai visto prima
aggiorna_conoscenza(st(_P,_G,T), _H, transizione(_S1,_A,_S2)) :-
	!,
	%  l'agente si guarda in giro e impara (ricorda)
	forall(trovato_loop(S,ListaStep),impara(ronda(S,ListaStep))),
	forall(stato_sentinella(S,P,D), impara(step_ronda(S,P,D,T))).

%  3) Per il cut, se arrivo qui non si ha nessuno dei casi precedenti,
%  non c'� nulla da imparare; l'agente non fa nulla
aggiorna_conoscenza(st(_P,_G,_T), _H, _Evento).

pred trovato_loop(id_sentinella,list(step_ronda(id_sentinella,punto,punto_cardinale,tempo))).
%%	trovato_loop(+S,-L) SEMIDET
%%	Spec: vero sse L e' una lista di step che descrivono un percorso
%	ripetuto dalla sentinella S
trovato_loop(S,ListaStep) :-
	not(conosce(ronda(S,_))),
	clock(T),
	stato_sentinella(S,P,D),
	conosce(step_ronda(S,P,D,Tprec)),
	setof(step_ronda(S,P1,D1,T1),
	      (	  conosce(step_ronda(S,P1,D1,T1)),between(Tprec,T,T1)),
	      ListaStep).

% assumibile � specificata in vai_if.pl; sono i predicati che
% l'agente potrebbe non conoscere e sui quali fa assunzioni
% il nostro agente esploratore fa assunzioni solo sulla mappa
% (per le posizioni che non ha ancora visto)
assumibile(step_ronda(_,_,_,_)).
assumibile(ronda(_,_)).

% contraria � specificata in vai_if.pl
% Due assunzioni sulla mappa sono contrarie se assumono due
% diverse qualit� di terreno su una stessa posizione
contraria(map(P,T1), map(P,T2)) :-
	T1 \= T2.

%  meta � specificata in vai_if.pl e indica i predicati sui quali
%  avviene il ragionamento basato su assunzioni con il predicato pensa;
% Il nostro agente pensa solo alla eseguibilit� delle azioni, quando
% decide raggiungibile e libera
meta(step_ronda(_,_,_,_)).

%  decide_se_assumere � specificata in vai_if.pl; quando l'agente
%  deve fare una assunzione, pu� decidere di non farla e fallire se
%  vi sono motivi in contrario
%  Il nostro esploratore �nottimista, assume SEMPTE che le posizioni che
%  non conosce siano libere
decide_se_assumere(step_ronda(S,P,D,_T)) :-
	stato_sentinella(S,P0,D),
	adiacenti(P0,Ad),
	member(P,Ad).


%  Visualizzazione della conoscenza in fase di debugging
%  mappa_agente USA position e goal della mappa corrente
%  ( specifica in livello.pl);
%  invece di usare map, usa ci� che conosce o ha assunto su map

/*
mappa_agente(P,Ch) :-
	position(P), !, Ch=x
	%  indico con x la posizione dell'agente
	;
	goal(P),!, Ch=g
	%  indico con g laposizione del goal
	;
	conosce(map(P,Ch)), !
	%  uso il carattere Ch della mappa, che l'agente conosce
	;
	assunto(map(P,_)), Ch='?'.
        %  indico con ? le posizioni che l'agente assume libere
        %  ma non conosce

mostra_conoscenza :-
	%  riscrivo mostra_conoscenza di default, forendo
	%  una rappresentazione grafica della conoscenza;
	%  uso mostra_mappa definita in livello.pl
	mostra_mappa(mappa_agente).

*/












