:- use_module(library(is_a)).
:- discontiguous(type(_)).
:- discontiguous(pred(_)).
:- discontiguous(gen(_)).


:- use_module(library(mr)).
:- use_module(library(pce)).
:- use_module(sentinella).
:- use_module(tempo).
:- use_module(gui).
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
     attesa,
       % decisione attesa: il percorso non e' sicuro, aspetto tre secondi per vedere se lo diventa
     avanzo(punto,tempo),
       % azione avanzo(P,T): avanzo nel punto indicato e nel tempo T
     aspetto,
       % azione aspetto: resto fermo per un secondo
     termino(evento)
    ]: decisione.

gen [map(punto, terreno)]: assumibile.

gen [risk, caution]: strat_assunzioni.

/***  A:  inizializzazioni  *****/

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
	carica_ronda(I),
	position(P0),
	goal(G),
	map_size(size(Width,Length)),
	% costruisco la finestra per la GUI
	dimensioni_finestra(Width,Length,X,Y),
	W is X + 24,
	L is Y + 24,
	% creo una coda in cui salvare i messaggi di attesa (relativi al tasto 'Next' della GUI)
	message_queue_create(next_move_queue),
	% creo la finestra e la popolo
	% new(@p, dialog('Agente Stealth', size(W,L))),
	% map_refresh(@p),
	% apro la finestra
	% send(@p,open),
	%send(@p, modal, transient),
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
decidi(st(DoveSono,G,_T),
       [eseguita(Dec)|_],
       Decisione)
:- DoveSono = G ->
   % se la mia posizione � il goal, termino
   Decisione = termino(eseguita(Dec)),
   message_queue_destroy(next_move_queue),
   free(@p)
   ;
   %  altrimenti cerco un piano per andare da DoveSono a G
   Decisione = vado(DoveSono,G).

% 3) nella ricerca del piano ho verificato l'impossibilita'
% di raggiungere il goal e mi ero gia' fermato ad aspettare
decidi(_ST,
       [impossibile(vado(P,G)),eseguita(attesa)|_],
       % termino tristemente
       termino(impossibile(vado(P,G)))):- !,
       message_queue_destroy(next_move_queue),
       free(@p).
% 4) nella ricerca del piano ho verificato l'impossibilita' di
% raggiungere il goal, ma forse se mi fermo le sentinelle si spostano
decidi(_ST,
       [impossibile(vado(_,_))|_],
       attesa).

% 5) nell'esecuzione del piano sono stato
% avvistato, quindi termino.
decidi(st(_DoveSono,_G,_T),
       [fallita(_,[Passo|_])|_],
       termino(impossibile(Passo))):-
       message_queue_destroy(next_move_queue),
       free(@p).

/****** C:   LE AZIONI  *******/

% azione � specificato in vai_if;  le azioni sono le seguenti:
azione(avanzo(_,_)).
azione(aspetto).

% esegui_azione � specificato in vai_if;
% faccio un passo verso la posizione indicata, dopo aver controllato se
% vengo avvistato o meno.
esegui_azione(st(P0,G,T0), _Storia, avanzo(P1,T0), st(P1,G,T1)) :-
	Tnext is T0 + 1,
	foreach(stato_sentinella(S,_,_),not(soldato_avvistato(S,P1,Tnext))),
	aggiorna_clock,
	clock(T1),
	retract(position(P0)),
	assert(position(P1)).
esegui_azione(st(P,G,T0), _Storia, aspetto, st(P,G,T1)) :-
	Tnext is T0 + 1,
	foreach(stato_sentinella(S,_,_),not(soldato_avvistato(S,P,Tnext))),
	aggiorna_clock,
	clock(T1).

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
	clock(Ora),
	cerca_un_piano(passo(P,Ora), passo(G,_), Piano).
piano(_ST, _Storia, attesa, [aspetto,aspetto,aspetto]).

pred marcati(list(punto)).
%%	marcati(?L) DET
%%	Spec: contiene la lista dei nodi espansi (necessaria perche'
%	l'introduzione del tempo rende complessa la potatura dell'albero
%	di ricerca
:-dynamic(marcati/1).

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
	assert(marcati([])),
	solve(P, Sol, =(G)),
	retractall(marcati(_)),
	estrai_piano(Sol,Piano).

type [nc(passo(punto,tempo),list(punto),number)]:nodo.
%  importo il tipo nc di search_spec.pl
pred estrai_piano(nodo, list(decisione)).
%  estrai_piano(+Sol, -Piano) det
%  Piano � la sequenza di avanzamenti che percorre la
%  sequenza di posizioni calcolata nella soluzione Sol
estrai_piano(nc(G,RevPath,_C), Piano) :-
	% faccio la reverse perch� il path � dal
	% nodo alla radice e quindi in senso inverso
	reverse([G|RevPath], [_Start|Path]),
	path2moves(Path, Piano).

pred path2moves(list(punto), list(decisione)).
path2moves([passo(P,T)|Path],[avanzo(P,T0)|MovList]) :-
	T0 is T - 1,
	path2moves(Path,MovList).
path2moves([],[]).


/*****  D1.  APPLICAZIONE DI A* e del ragionamento basato su
             assunzioni nella ricerca di un piano

	     A* � implementato nel modulo mr.pl e richiede
	     di implementare l'interfaccia search_if.pl

	     Il predicato pensa � implementato in vai.pl

*************************************************************/

%  implemento vicini, richiesto dall'interfaccia search_if
vicini(P, ListaVicini) :-
	P = passo(Pos,_),
	retract(marcati(L)),
	assert(marcati([Pos|L])),
	setof(P1, pensa_sicuro(P, P1), ListaVicini),
	!
	;
	ListaVicini=[].
/*
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
*/

pred pensa_sicuro(punto,punto,tempo).
%  pensa_sicuro(+P1, -P2) nondet
%  P2 e' raggiunibile da P1 ed e' un punto sicuro in base a quanto
%  assume l'agente nello stato di conoscenza attuale
pensa_sicuro(passo(P1,T1),passo(P2,T2)) :-
	adiacenti(P1,ListaAdiacenti),
	member(P2,ListaAdiacenti),
	libera(P2),
	marcati(Espansi),
	not(member(P2,Espansi)),
	T2 is T1 + 1,
	not(pensa_avvistato(_,P2,T2)).

pred pensa_avvistato(id_sentinella,punto,tempo).
%%	pensa_avvistato(?S,?P,?T) SEMIDET
%%	Spec: vero sse l'agente pensa di essere avvistato da S nel punto
%	P al tempo T
pensa_avvistato(_S,P,T) :-
	setof(Sent,(stato_sentinella(Sent,_,_),pensa(punto_sorvegliato(Sent,P,T),_)),ListaSorvegliati),
  length(ListaSorvegliati,L),
  L >= 1.

%  il costo di un passo e' sempre 1
costo(P1,P2, 1) :-
	P1 \== P2.
% implemento l'euristica usando la distanza euclidea
h(passo(P,T),H) :-
	current_goal(passo(G,_)),
	distanza_euclidea(P,G,H1),
	H is H1 + T.




/******************  E)  PARTE DI RAGIONAMENTO  *******************/

%%	predicato usato nell'aggiornamento della conoscenza
pred impara_punti_sorvegliati(tempo).
%%	impara_punti_sorvegliati(+T) DET
%%	Spec: impara quali punti sono sorvegliati al tempo T
impara_punti_sorvegliati(T) :-
	forall(stato_sentinella(S,_,_),(estrai_punti_area(S,L),
					forall(member(Pa,L),(
						   not(conosce(punto_sorvegliato(S,Pa,T))) ->
						   impara(punto_sorvegliato(S,Pa,T)))))).

%   aggiorna_conoscenza � specificata in vai_if.pl
%   avviene a fronte di un evento verificatosi
%
%  1) evento inizio_storia(_). All'inizio l'agente si trova in una
%  posizione che, almeno nella prima escuzione, � nuova e per prima cosa
%  memorizza i punti sorvegliati
aggiorna_conoscenza(st(_P,_G,_), _H, inizio_storia(_Avvio)) :-
	%  l'agente si guarda in giro e impara (ricorda) le posizioni iniziali dei        %  punti sorvegliati dalle sentinelle
	impara_punti_sorvegliati(0),
	!.

%   2) evento transizione(S1,A,S2,PL). E' stata eseguita la transizione
%   da S1 a S2, l'agente si trova in una posizione nuova in un certo
%   tempo
aggiorna_conoscenza(st(_P,_G,_T), _H, transizione(_S1,_A,_S2)) :-
	%  l'agente si guarda in giro e memorizza i punti sorvegliati
	clock(T),
	impara_punti_sorvegliati(T),
	!.

%   3) evento fallita(vado(_,_),[_]). Mentre l'agente si spostava, e'
%   stato avvistato. L'agente dunque memorizza la posizione in cui e'
%   stato avvistato, in modo da ricordarsene durante la prossima
%   esecuzione.
aggiorna_conoscenza(st(_,G,_), _H, fallita(vado(_,G),[avanzo(Dest,_)|_])) :-
	clock(T),
	Tnext is T + 1,
	soldato_avvistato(S,Dest,Tnext),
	not(conosce(punto_sorvegliato(S,Dest,Tnext))) ->
	impara(punto_sorvegliato(S,Dest,Tnext)),
	!.

%  4) Per il cut, se arrivo qui non si ha nessuno dei casi precedenti,
%  non c'� nulla da imparare; l'agente non fa nulla
aggiorna_conoscenza(st(_P,_G,_T), _H, _Evento).

pred estrai_punti_area(id_sentinella,list(punto)).
%%	estrai_punti_area(+A,-L) DET
%%	Spec: vero sse L e' la lista dei punti interni all'area A
estrai_punti_area(S,L) :-
	stato_sentinella(S,P,D),
	area_sentinella(P,D,A),
	map_size(Mappa),
	setof(Punto,(punto_mappa(Punto,Mappa),punto_area(Punto,A)),L).

% assumibile � specificata in vai_if.pl; sono i predicati che
% l'agente potrebbe non conoscere e sui quali fa
% assunzioni.
% L'agente fa assunzioni sui punti sorvegliati dalle sentinelle.
assumibile(punto_sorvegliato(_,_,_)).

% contraria � specificata in vai_if.pl
% Nessuna coppia di assunzioni e' inconsistente: l'agente assume piu' posizioni
% nello stesso tempo per considerare anche il non determinismo.
contraria(_,_) :- false.

%  meta � specificata in vai_if.pl e indica i predicati sui quali
%  avviene il ragionamento basato su assunzioni con il predicato pensa;
% Il nostro agente pensa alla possibilita' che un punto sia sorvegliato
% da una sentinella ad un certo tempo.
meta(punto_sorvegliato(_,_,_)).

pred strategia_assunzioni(strat_assunzioni).
%%	strategia_assunzioni(-S) DET
%%	Spec: vero sse S e' la strategia attualmente adottata
%	dall'agente per le assunzioni
:-dynamic(strategia_assunzioni/1).
strategia_assunzioni(risk). % valore di default

pred set_strategia_assunzioni(strat_assunzioni).
%%	set_strategia_assunzioni(++S) DET
%%	Spec: imposta la strategia per le assunzioni
set_strategia_assunzioni(S) :-
	member(S,[risk,caution]),
	retractall(strategia_assunzioni(_)),
	assert(strategia_assunzioni(S)).

%  decide_se_assumere � specificata in vai_if.pl; quando l'agente
%  deve fare una assunzione, pu� decidere di non farla e fallire se
%  vi sono motivi in contrario
%  L'agente assume sempre che ogni sentinella, nell'immediato futuro,
%  estendera' la propria area di influenza nella direzione in cui sta
%  guardando.
%  Assunzioni in modalita' risk
mappa_ronda(4,3) :- !.
mappa_ronda(5,2) :- !.
mappa_ronda(6,1) :- !.
mappa_ronda(I,I).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(risk),
	libera(P),
	stato_sentinella(S,Psent,e), !,
	area_sentinella(Psent,e,area(p(I1,J1),p(I2,J2))),
	clock(Ora),
	Diff is T - Ora,
	Spostamento is Diff mod 7,
	mappa_ronda(Spostamento,S1),
	J1nuovo is J1 + S1,
	J2nuovo is J2 + S1,
	punto_area(P,area(p(I1,J1nuovo),p(I2,J2nuovo))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(risk),
	libera(P),
	stato_sentinella(S,Psent,o), !,
	area_sentinella(Psent,o,area(p(I1,J1),p(I2,J2))),
	clock(Ora),
	Diff is T - Ora,
	Spostamento is Diff mod 7,
	mappa_ronda(Spostamento,S1),
	J1nuovo is J1 - S1,
	J2nuovo is J2 - S1,
	punto_area(P,area(p(I1,J1nuovo),p(I2,J2nuovo))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(risk),
	libera(P),
	stato_sentinella(S,Psent,n), !,
	area_sentinella(Psent,n,area(p(I1,J1),p(I2,J2))),
	clock(Ora),
	Diff is T - Ora,
	Spostamento is Diff mod 7,
	mappa_ronda(Spostamento,S1),
	I1nuovo is I1 - S1,
	I2nuovo is I2 - S1,
	punto_area(P,area(p(I1nuovo,J1),p(I2nuovo,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(risk),
	libera(P),
	stato_sentinella(S,Psent,s), !,
	area_sentinella(Psent,s,area(p(I1,J1),p(I2,J2))),
	clock(Ora),
	Diff is T - Ora,
	Spostamento is Diff mod 7,
	mappa_ronda(Spostamento,S1),
	I1nuovo is I1 + S1,
	I2nuovo is I2 + S1,
	punto_area(P,area(p(I1nuovo,J1),p(I2nuovo,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).


% Assunzioni in modalita' caution
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(caution),
	clock(Ora),
	Diff is T - Ora,
	(Diff > 3 -> M is 3; M is Diff),
	libera(P),
	stato_sentinella(S,Psent,e), !,
	area_sentinella(Psent,e,area(p(I1,J1),p(I2,J2))),
	J1nuovo is J1 + M,
	punto_area(P,area(p(I1,J1nuovo),p(I2,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(caution),
	libera(P),
	clock(Ora),
	Diff is T - Ora,
	(Diff > 3 -> M is 3; M is Diff),
	stato_sentinella(S,Psent,o), !,
	area_sentinella(Psent,o,area(p(I1,J1),p(I2,J2))),
	J2nuovo is J2 - M,
	punto_area(P,area(p(I1,J1),p(I2,J2nuovo))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(caution),
	libera(P),
	clock(Ora),
	Diff is T - Ora,
	(Diff > 3 -> M is 3; M is Diff),
	stato_sentinella(S,Psent,n), !,
	area_sentinella(Psent,n,area(p(I1,J1),p(I2,J2))),
	I1nuovo is I1 - M,
	punto_area(P,area(p(I1nuovo,J1),p(I2,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).
decide_se_assumere(punto_sorvegliato(S,P,T)) :-
	strategia_assunzioni(caution),
	libera(P),
	clock(Ora),
	Diff is T - Ora,
	(Diff > 3 -> M is 3; M is Diff),
	stato_sentinella(S,Psent,s), !,
	area_sentinella(Psent,s,area(p(I1,J1),p(I2,J2))),
	I2nuovo is I2 + M,
	punto_area(P,area(p(I1,J1),p(I2nuovo,J2))),
	assert(assunto(punto_sorvegliato(S,P,T))).

%  Visualizzazione della conoscenza in fase di debugging
%  mappa_agente USA position e goal della mappa corrente
%  ( specifica in livello.pl);
%  invece di usare map, usa ci� che conosce o ha assunto su map

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
	% map_refresh(@p),
	mostra_mappa(mappa_agente).
