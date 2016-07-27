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

% specificato in vai_if; l'informazione iniziale è qui
% la mappa che vogliamo usare; se già caricata non viene
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

% fine è specificato in vai_if; la decisione finale è termino(...)
fine(termino(_)).

% decidi è specificato in vai_if;  lo implemento come segue
% 1) sono a inizio storia con goal G;
decidi(st(P0,G,_T),
       [inizio_storia(_I)],
       % decido di cercare un piano per andare da P0 a G
       vado(P0,G)).

% 2) ho eseguito la decisione precedente _Dec;
decidi(st(DoveSono,G,_T),
       [eseguita(Dec)|_],
       Decisione)
:- DoveSono = G ->
   % se la mia posizione è il goal, termino
   Decisione = termino(eseguita(Dec))
   ;
   %  altrimenti cerco un piano per andare da DoveSono a G
   Decisione = vado(DoveSono,G).

% 3) nella ricerca del piano ho verificato l'impossibilita'
% di raggiungere il goal
decidi(_ST,
       [impossibile(vado(P,G))|_],
       % termino tristemente
       termino(impossibile(vado(P,G)))).

% 4) nell'esecuzione del piano sono stato avvistato: faccio un passo
% indietro e ripianifico.
decidi(st(_DoveSono,G,_T),
       [fallita(vado(_P0,G),[avanzo(P,Tprec)|_Storia])|_],
       termino(impossibile(avanzo(P,Tprec)))).

/****** C:   LE AZIONI  *******/

% azione è specificato in vai_if;  le azioni sono le seguenti:
azione(avanzo(_,_)).

% esegui_azione è specificato in vai_if;
% la posizione in cui devo andare è visibile; se libera
% ci vado, altrimenti fallisco
esegui_azione(st(P0,G,T0), _Storia, avanzo(P1,T0), st(P1,G,T1)) :-
	aggiorna_clock,
	clock(T1),
	retract(position(P0)),
	assert(position(P1)),
	foreach(stato_sentinella(S,_,_),not(soldato_avvistato(S,P1,T1))).

pred libera(punto).
   %  libera(?P) semidet:    P è una posizione della mappa
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
%  Piano è un piano che porta daP a G, possibile in base alle
%  conoscenze che l'agente ha in questo momento
%  FALLISCE se non ci sono piani, nel qual caso l'agente sa che il goal
%  non è raggiungibile indipendentemente da ciò che ancora non conosce
:- dynamic(current_goal/1).
pred current_goal(punto).
cerca_un_piano(P, G, Piano) :-
	%  RICORDO IL GOAL DI QUESTO PIANO PER USARLO
	%  NELLA EURISTICA che usa la distanza quadretti da G
	retractall(current_goal(_)),
	assert(current_goal(G)),
	%   USO solve imlementato nel modulo ricerca mr.pl
	solve(P, Sol, =(G)),
	estrai_piano(Sol,Piano).

type [nc(punto,list(punto),number)]:nodo.
%  non stiamo a importare tutti i tipi di search_spec.pl
%  qui ci basta questo
pred estrai_piano(nodo, list(decisione)).
%  estrai_piano(+Sol, -Piano) det
%  Piano è la sequenza di avanzamenti che percorre la
%  sequenza di posizioni calcolata nella soluzione Sol
estrai_piano(nc(G,RevPath,_C), Piano) :-
	% faccio la reverse perchè il path è dal
	% nodo alla radice e quindi in senso inverso
	reverse([G|RevPath], [_Start|Path]),
	clock(T),
	path2moves(Path, Piano, T).

pred path2moves(list(punto), list(decisione),tempo).
path2moves([P|Path],[avanzo(P,T)|MovList],T) :-
	Tnext is T + 1,
	path2moves(Path,MovList,Tnext).
path2moves([],[],_).


/*****  D1.  APPLICAZIONE DI A* e del ragionamento basato su
             assunzioni nella ricerca di un piano

	     A* è implementato nel modulo mr.pl e richiede
	     di implementare l'interfaccia search_if.pl

	     Il predicato pensa è implementato in vai.pl

*************************************************************/

%  implemento vicini, richiesto dall'interfaccia search_if
vicini(P, V) :-
	tempo_di_percorrenza(P,T),
	setof(P1, pensa_sicuro(P, P1, T), V),
	!
	;
	V=[].

pred tempo_di_percorrenza(punto,tempo).
%%	tempo_di_percorrenza(+P,-T) DET
%%	Spec: vero sse T e' il tempo che l'agente ipotizza impieghera' a
%	percorrere la distanza che lo separa da P
tempo_di_percorrenza(P,T) :-
	position(P0),
	manhattan(P,P0,M),
	clock(T0),
	T is T0 + M.
manhattan(p(I1,J1),p(I2,J2),M) :-
	M is abs(I1 - I2) + abs(J1 - J2).

pred pensa_sicuro(punto,punto,tempo).
%  pensa_sicuro(+P1, -P2) nondet
%  P2 è raggiunibile da P1 ed e' un punto sicuro in base a quanto assume
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
	pensa(punto_sorvegliato(S,P,T),_).

%  implemento il costo, in base a quanto assume l'agente
%  nello stato di conoscenza attuale
costo(P1,P2, 1) :-
	P1 \== P2.
% implemento l'euristica usando la distanza in quadretti
% implementata in livello.pl; il goal è quello memorizzato
% prima di lanciare la ricerca con solve
h(P,H) :-
	current_goal(G),
	distanza_euclidea(P,G,H).




/******************  E)  PARTE DI RAGIONAMENTO  *******************/

%   aggiorna_conoscenza è specificata in vai_if.pl
%   avviene a fronte di un evento verificatosi

%  1) evento inizio_storia(_). All'inizio l'agente si trova in una
%  posizione che, almeno nella prima escuzione, è nuova
aggiorna_conoscenza(st(_P,_G,_), _H, inizio_storia(_Avvio)) :-
	%  l'agente si guarda in giro e impara (ricorda) cosa
	%  c'è nel mondo nelle posizioni adiacenti
	not(conosce(punto_sorvegliato(_,_,0))), !,
	forall(stato_sentinella(S,_,_),(estrai_punti_area(S,L),
					forall(member(Pa,L),
					       impara(punto_sorvegliato(S,Pa,0))))).

%   2) evento transizione(S1,A,S2,PL). E' stata eseguita la transizione
%   da S1 a S2, l'agente si trova in una posizione che potrebbe non aver
%   mai visto prima
aggiorna_conoscenza(st(_P,_G,_T), _H, transizione(_S1,_A,_S2)) :-
	!,
	%  l'agente si guarda in giro e impara (ricorda)
	clock(T),
	not(conosce(punto_sorvegliato(_,T))),
	setof(A,(stato_sentinella(_,P,D),area_sentinella(P,D,A)),ListaAree),
	forall(member(Area,ListaAree),(punto_area(Punto,Area),
				       impara(punto_sorvegliato(s1,Punto,T)))).

aggiorna_conoscenza(st(_,G,_), _H, fallita(vado(_,G),[avanzo(Dest,Tprec)|_])) :-
	% NOTA: bisogna sapere quale sentinella mi ha visto
	T is Tprec + 1,
	impara(punto_sorvegliato(s1,Dest,T)).

%  3) Per il cut, se arrivo qui non si ha nessuno dei casi precedenti,
%  non c'è nulla da imparare; l'agente non fa nulla
aggiorna_conoscenza(st(_P,_G,_T), _H, _Evento).

pred estrai_punti_area(id_sentinella,list(punto)).
%%	estrai_punti_area(+A,-L) DET
%%	Spec: vero sse L e' la lista dei punti interni all'area A
estrai_punti_area(S,L) :-
	stato_sentinella(S,P,D),
	area_sentinella(P,D,A),
	setof(Punto,punto_area(Punto,A),L).

% assumibile è specificata in vai_if.pl; sono i predicati che
% l'agente potrebbe non conoscere e sui quali fa assunzioni
% il nostro agente esploratore fa assunzioni solo sulla mappa
% (per le posizioni che non ha ancora visto)
assumibile(punto_sorvegliato(_,_,_)).

% contraria è specificata in vai_if.pl
% Due assunzioni sulla mappa sono contrarie se assumono due
% diverse qualità di terreno su una stessa posizione

contraria(_,_):- false.
%  meta è specificata in vai_if.pl e indica i predicati sui quali
%  avviene il ragionamento basato su assunzioni con il predicato pensa;
% Il nostro agente pensa solo alla eseguibilità delle azioni, quando
% decide raggiungibile e libera
meta(punto_sorvegliato(_,_,_)).

%  decide_se_assumere è specificata in vai_if.pl; quando l'agente
%  deve fare una assunzione, può decidere di non farla e fallire se
%  vi sono motivi in contrario
%  Il nostro esploratore ènottimista, assume SEMPTE che le posizioni che
%  non conosce siano libere
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	libera(P),
	stato_sentinella(S,Psent,e), !,
	area_sentinella(Psent,e,area(p(I1,J1),p(I2,J2))),
	J1nuovo is J1 + 1,
	J2nuovo is J2 + 1,
	punto_area(P,area(p(I1,J1nuovo),p(I2,J2nuovo))),
	assert(assunto(punto_sorvegliato(P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	libera(P),
	stato_sentinella(S,Psent,o), !,
	area_sentinella(Psent,o,area(p(I1,J1),p(I2,J2))),
	J1nuovo is J1 - 1,
	J2nuovo is J2 - 1,
	punto_area(P,area(p(I1,J1nuovo),p(I2,J2nuovo))),
	assert(assunto(punto_sorvegliato(P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	libera(P),
	stato_sentinella(S,Psent,n), !,
	area_sentinella(Psent,n,area(p(I1,J1),p(I2,J2))),
	I1nuovo is I1 - 1,
	I2nuovo is I2 - 1,
	punto_area(P,area(p(I1nuovo,J1),p(I2nuovo,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	libera(P),
	stato_sentinella(S,Psent,s), !,
	area_sentinella(Psent,s,area(p(I1,J1),p(I2,J2))),
	I1nuovo is I1 + 1,
	I2nuovo is I2 + 1,
	punto_area(P,area(p(I1nuovo,J1),p(I2nuovo,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).



%  Visualizzazione della conoscenza in fase di debugging
%  mappa_agente USA position e goal della mappa corrente
%  ( specifica in livello.pl);
%  invece di usare map, usa ciò che conosce o ha assunto su map


mappa_agente(P,Ch) :-
	position(P), !, Ch=x
	%  indico con x la posizione dell'agente
	;
	goal(P),!, Ch=p
	%  indico con g laposizione del goal
	;
	map(P,'o'),!, Ch=o
	;
	stato_sentinella(_,Ps,Ds),
	area_sentinella(Ps,Ds,A),
	punto_area(P,A),!,
	Ch = '*'.

mostra_conoscenza :-
	%  riscrivo mostra_conoscenza di default, forendo
	%  una rappresentazione grafica della conoscenza;
	%  uso mostra_mappa definita in livello.pl
	mostra_mappa(mappa_agente).














