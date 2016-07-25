:- use_module(library(is_a)).
:- discontiguous(type(_)).
:- discontiguous(pred(_)).
:- discontiguous(gen(_)).


:- use_module(library(mr)).
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
pred P :- vai_spec:pred(P).
pred P :- vai_if:pred(P).
pred P :- livello_spec:pred(P).



/***   IMPLEMENTO I TIPI DI vai_if ******/

gen [mappa(number)]:info_inizio.
    % mappa(I):  identifica la mappa caricabile n. I

gen [st(punto, punto)]:stato.
    % st(P,G): l'agente si trova in P e deve andare a G


/******   Le decisioni dell'agente  */
gen [vado(punto, punto),
       % decisione vado(P,G): cerco un piano per andare da P a G
     aggiramento(punto,punto,punto,list(decisione)),
       % decisione aggiramento(Pos, Ostacolo,Goal,Piano): sono in Pos, devo andare
       % in Goal, cerco un piano per aggirare l'Ostacolo appena incontrato
     aggiro(punto,punto),
       % azione aggiro(P,O): avanzo in P aggirando O
     avanzo(punto),
       % azione avanzo(P): avanzo nel punto indicato
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

stato_iniziale(st(P0,G), mappa(I)) :-
	strategy(astar),
	strategy(pota_chiusi),
	carica_mappa(I),
	position(P0),
	goal(G),
	(   ultima(I) ->
	    writeln('CONTUNUAZIONE SU ':mappa(I))
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
decidi(st(P0,G),
       [inizio_storia(_I)],
       % decido di cercare un piano per andare da P0 a G
       vado(P0,G)).

% 2) ho eseguito la decisione precedente _Dec;
decidi(st(DoveSono,G),
       [eseguita(Dec)|_],
       Decisione)
:- DoveSono=G ->
   % se la mia posizione è il goal, termino
   Decisione=termino(eseguita(Dec))
   ;
   %  altrimenti cerco un piano per andare da DoveSono a G
   Decisione = vado(DoveSono,G).

% 3) nella ricerca del piano ho verificato l'impossibilita'
% di raggiungere il goal
decidi(_ST,
       [impossibile(vado(P,G))|_],
       % termino tristemente
       termino(impossibile(vado(P,G)))).

% 4) devo aggirare un ostacolo, ma non è possibile un
% semplice piano di aggiramento
decidi(_ST,
       [impossibile(aggiramento(P,_O, G,_Piano))|_],
       % da P non posso aggirare O, decido di cercare un piano per
       % andare da P a G
       vado(P,G)).

% 5A) nella esecuzione di un piano, ho trovato un ostacolo aggirabile
decidi(st(DoveSono,_),
       [fallita(vado(_,G),[avanzo(Ostacolo)|Piano])|_],
       % se da quel che vedo l'ostacolo potrebbe essere aggirato,
       % prima di ripianificare decido di tentare un aggiramento
       aggiramento(DoveSono,Ostacolo, G, Piano)
       % nella decisione ricordo l'Ostacolo, il Goal e
       % il piano da riprendere se riesco ad aggirare l'Ostacolo
      )
:- aggirabile(DoveSono,Ostacolo).
% 5B) nella esecuzione di un piano, ho trovato un ostacolo non
% aggirabile
decidi(st(DoveSono,_),
       [fallita(vado(_,G),[avanzo(Ostacolo)|_])|_],
       % se da quel che vedo l'ostacolo potrebbe essere aggirato,
       % prima di ripianificare decido di tentare un aggiramento
       vado(DoveSono,G)
       % nella decisione ricordo l'Ostacolo, il Goal e
       % il piano da riprendere se riesco ad aggirare l'Ostacolo
      )
:- not(aggirabile(DoveSono,Ostacolo)).

% 6) nel tentativo di aggiramento trovo un ostacolo
decidi(_ST,
       [fallita(aggiramento(P,_, G, _),_)|_],
       % a questo punto ripianifico
       vado(P,G)).

pred aggirabile(punto, punto).
%  aggirabile(+DoveSono,+Ostacolo) semidet:
%  Ostacolo è aggirabile a partire da DoveSono
%  proseguendo in una delle due direzioni prossime a
%  quella che stavo percorrendo verso Ostacolo
%  Ad es.: le direzioni prossime di nord sono nord-est
%  e nord-ovest; uso next_dirs implementato in livello.pl
aggirabile(P, Ostacolo) :-
	aggirabile(P,Ostacolo,_),!.

pred aggirabile(punto, punto,punto).
%  aggirabile(+DoveSono,+Ostacolo,-DoveVado) semidet:
%  Ostacolo è aggirabile a partire da DoveSono a DoveVado
%  proseguendo in una delle due direzioni prossime
aggirabile(P, Ostacolo, O) :-
	next(P, Dir, Ostacolo, _),
	next_dirs(Dir, Dir1, Dir2),
	(   next(P,Dir1,O,_),
	    libera(O)
	    ;
	    next(P,Dir2,O,_),
	    libera(O)
	 ).

/****** C:   LE AZIONI  *******/

% azione è specificato in vai_if;  le azioni sono le seguenti:
azione(avanzo(_)).
azione(aggiro(_,_)).

% esegui_azione è specificato in vai_if;
% la posizione in cui devo andare è visibile; se libera
% ci vado, altrimenti fallisco
esegui_azione(st(P0,G), _Storia, avanzo(P1), st(P1,G)) :-
	raggiungibile(P0,P1,_),
	retract(position(P0)),
	assert(position(P1)).
esegui_azione(st(P0,G), _Storia, aggiro(_,P1), st(P1,G)) :-
	raggiungibile(P0,P1,_),
	retract(position(P0)),
	assert(position(P1)).

pred raggiungibile(punto, punto, number).
%  raggiungibile(+P1, ?P2, ?L) nondet:
%  posso andare da P1 a P2 (che è libero) in un passo
%  con lunghezza L
raggiungibile(P0, P1, L) :-
	next(P0,_Dir,P1,L),
	libera(P1).

pred libera(punto).
   %  libera(?P) semidet:    P è una posizione della mappa
   %  libera da ostacoli
libera(P) :-
	map(P,T),
	not(T=o).

/*****  D) La pianificazione delle decisioni non direttamente
 *         eseguibili come azioni                     ********/

% 1) ho deciso di cercare un piano per andare da P a G, lo cerco
piano(_ST, _Storia, vado(P,G), Piano):-
	cerca_un_piano(P, G, Piano).

% 2) ho deciso di tentare un aggiramento, lo pianifico
piano(_ST, _Storia,
      aggiramento(Posiz, Ostacolo, _G, Piano), PianoAggiramento)
:- cerca_aggiramento(Posiz, Ostacolo, Piano, PianoAggiramento).


pred cerca_aggiramento(punto, punto, list(decisione), list(decisione)).
%   cerca_aggiramento(+DoveSono, +Ostacolo,+Piano1, -Piano2) det.
%   Precondizione:  da DoveSono Ostacolo sembra aggirabile e Piano1 è
%   il piano da riprendere dopo l'aggiramento
%   Postcondizione: Piano2 è il piano da eseguire per aggirare
%   l'ostacolo e poi proseguire con il piano interrotto
cerca_aggiramento(P, O, [avanzo(Q)|Piano],
		  [aggiro(O,P1),avanzo(Q)|Piano]) :-
	aggirabile(P, O, P1),
	next(P1,Q).
cerca_aggiramento(P, O, [avanzo(Q)|Piano],
		  [aggiro(O,P1),aggiro(O,P2),avanzo(Q)|Piano]) :-
	aggirabile(P, O, P1),
	next(P1,P2),
	not(P2=O),
	next(P2,Q).

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
	path2moves(Path, Piano).

pred path2moves(list(punto), list(decisione)).
path2moves([P|Path],[avanzo(P)|MovList]) :-
	path2moves(Path,MovList).
path2moves([],[]).


/*****  D1.  APPLICAZIONE DI A* e del ragionamento basato su
             assunzioni nella ricerca di un piano

	     A* è implementato nel modulo mr.pl e richiede
	     di implementare l'interfaccia search_if.pl

	     Il predicato pensa è implementato in vai.pl

*************************************************************/

%  implemento vicini, richiesto dall'interfaccia search_if
vicini(P, V) :-
	setof(P1, pensa_vicino(P, P1), V), !; V=[].

pred pensa_vicino(punto,punto).
%  pensa_vicino(+P1, -P2) nondet
%  P2 è raggiunibile da P1 in base a quanto assume l'agente
%  nello stato di conoscenza attuale
pensa_vicino(P1,P2) :-
	pensa(raggiungibile(P1,P2,_),_).

%  implemento il costo, in base a quanto assume l'agente
%  nello stato di conoscenza attuale
costo(P1, P2, C) :-
	pensa(raggiungibile(P1,P2,D),_),
	pensa(map(P1,T1),_),
	pensa(map(P2,T2),_),
	qualita(T1,K1),
	qualita(T2,K2),
	C is D*(K1+K2)/2.

% implemento l'euristica usando la distanza in quadretti
% implementata in livello.pl; il goal è quello memorizzato
% prima di lanciare la ricerca con solve
h(P,H) :-
	current_goal(G),
	distanza_quadretti(P,G,H).




/******************  E)  PARTE DI RAGIONAMENTO  *******************/

%   aggiorna_conoscenza è specificata in vai_if.pl
%   avviene a fronte di un evento verificatosi

%  1) evento inizio_soria(_). All'inizio l'agente si trova in una
%  posizione che, almeno nella prima escuzione, è nuova
aggiorna_conoscenza(st(P,_G), _H, inizio_storia(_Avvio)) :- !,
	%  l'agente si guarda in giro e impara (ricorda) cosa
	%  c'è nel mondo nelle posizioni adiacenti
	forall(next(P, AP), (map(AP,T), impara(map(AP,T)))).

%   2) evento transizione(S1,A,S2,PL). E' stata eseguita la transizione
%   da S1 a S2, l'agente si trova in una posizione che potrebbe non aver
%   mai visto prima
aggiorna_conoscenza(st(_P,_G), _H, transizione(_S1,_A,S2)) :- !,
	S2 = st(P2,_),
	%  l'agente si guarda in giro e impara (ricorda)
	forall(next(P2, AP), (map(AP,T), impara(map(AP,T)))).

%  3) Per il cut, se arrivo qui non si ha nessuno dei casi precedenti,
%  non c'è nulla da imparare; l'agente non fa nulla
aggiorna_conoscenza(st(_P,_G), _H, _Evento).

% assumibile è specificata in vai_if.pl; sono i predicati che
% l'agente potrebbe non conoscere e sui quali fa assunzioni
% il nostro agente esploratore fa assunzioni solo sulla mappa
% (per le posizioni che non ha ancora visto)
assumibile(map(_,_)).

% contraria è specificata in vai_if.pl
% Due assunzioni sulla mappa sono contrarie se assumono due
% diverse qualità di terreno su una stessa posizione
contraria(map(P,T1), map(P,T2)) :-
	T1 \= T2.

%  meta è specificata in vai_if.pl e indica i predicati sui quali
%  avviene il ragionamento basato su assunzioni con il predicato pensa;
% Il nostro agente pensa solo alla eseguibilità delle azioni, quando
% decide raggiungibile e libera
meta(raggiungibile(_,_,_)).
meta(libera(_)).

%  decide_se_assumere è specificata in vai_if.pl; quando l'agente
%  deve fare una assunzione, può decidere di non farla e fallire se
%  vi sono motivi in contrario
%  Il nostro esploratore ènottimista, assume SEMPTE che le posizioni che
%  non conosce siano libere
decide_se_assumere(map(P,' ')) :-
	assert(assunto(map(P,' '))).



%  Visualizzazione della conoscenza in fase di debugging
%  mappa_agente USA position e goal della mappa corrente
%  ( specifica in livello.pl);
%  invece di usare map, usa ciò che conosce o ha assunto su map

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














