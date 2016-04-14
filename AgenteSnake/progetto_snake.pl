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

gen [mappa(number)]: info_inizio.
	% mappa(I):  identifica la mappa caricabile n. I

gen [st(punto, punto)]:stato.
    % st(P,G): il soldato si trova in P e deve andare a G
    % NOTA: possibili aggiunte di termini allo stato
         % tempo: perché le posizioni dei nemici cambiano a seconda del tempo
         % visibilità: da usare se si implementa la possibilità di nascondersi

/******   Le decisioni dell'agente  */
gen [ % piani
	 vado(punto, punto),
       % decisione vado(P,G): cerco un piano per andare da P a G
    % mi_nascondo(punto),
       % decisione mi_nascondo(P): cerco un piano per nascondermi nel punto P

     % azioni
    % entro_nascondiglio(punto),
       % azione entro_nascondiglio(P): mi nascondo nel punto P e divento invisibile
    % esco_nascondiglio(punto),
       % azione esco_nascondiglio(P): esco dal nascondiglio in P e torno visibile
     aspetto,
       % azione aspetto: sto fermo per un turno
     avanzo(punto),
       % azione avanzo(P): avanzo nel punto indicato
     termino(evento)
    ]: decisione.

gen [ronda(sentinella,punto,direzione,integer)]: assumibile.
% NOTA: forse anche guardia/2 dovrebbe essere assumibile, ma in teoria basta
%  assumere il pattern

pred clear_knowledge.
% comando qui introdotto per azzerare la conoscenza, che altrimenti
% resta acquisita ad ogni esecuzione e si accumula

clear_knowledge :-
	retractall(assunto(_)),
	retractall(conosce(_)).

% specificato in vai_if; l'informazione iniziale Ã¨ qui
% la mappa che vogliamo usare; se giÃ  caricata non viene
% azzerata conoscenza dinamica dell'agente, che di volta in volta
% impara; se si passa a nuova mappa, l'agente parte con
% conoscenza dinamica nulla;  usiamo  ultima/1 per ricordare
% l'ultima mappa caricata
:- dynamic(ultima/1).

stato_iniziale(st(S0,P), mappa(I)) :-
	strategy(astar),
	strategy(pota_chiusi),
	carica_mappa(I),
	soldato(S0),
	prigioniero(P),
	(   ultima(I) ->
	    writeln('CONTINUAZIONE SU ':mappa(I))
	;   retractall(ultima(_)),
	    clear_knowledge,
	    writeln('NUOVA ':mappa(I)),
	    assert(ultima(I))
	).

/****  B: le decisioni *******/

% fine Ã¨ specificato in vai_if; la decisione finale Ã¨ termino(...)
fine(termino(_)).

% decidi Ã¨ specificato in vai_if;  lo implemento come segue
% 1) sono a inizio storia con stato st(S,P);
decidi(st(Soldato,Prigioniero),
       [inizio_storia(_I)],
       % decido di cercare un piano per andare da P0 a G
       vado(Soldato,Prigioniero)).

% 2) ho eseguito la decisione precedente _Dec;
decidi(st(Soldato,Prigioniero),
       [eseguita(Dec)|_],
       Decisione)
:- Soldato = Prigioniero ->
   % se la mia posizione è il goal, termino
   Decisione = termino(eseguita(Dec))
   ;
   %  altrimenti cerco un piano per andare da Soldato a Prigioniero
   Decisione = vado(Soldato, Prigioniero).

% 3) nella ricerca del piano ho verificato che il prigioniero è
% irraggiungibile oppure non si può raggiungere evitando le guardie;
decidi(_ST,
       [impossibile(vado(Soldato,Prigioniero))|_],
       % termino per impossibilità di raggiungere il goal
       termino(impossibile(vado(Soldato,Prigioniero)))).

% 4A) sono stato visto da una guardia mentre tentavo di raggiungiere il
% prigioniero;
decidi(st(_Soldato,_),
       [fallita(vado(S,P),[avanzo(_)|_])|_],
       termino(fallita(vado(S,P))))
:- avvistato(_Sentinella).
% 4B) sono stato visto da una guardia mentre aspettavo in un punto.
decidi(st(_Soldato,_),
       [fallita(vado(S,P),[aspetto|_])|_],
       termino(fallita(vado(S,P))))
:- avvistato(_Sentinella).

%%	NOTA: da implementare
pred avvistato(sentinella).
%%	avvistato(-S) SEMIDET
%%	Spec: vero sse S è la sentinella che ha avvistato l'agente

/**** AZIONI ****/

azione(avanzo(_)).
azione(aspetto).
azione(termino(_)).

% Specifica in vai_if.pl
esegui_azione(st(S0,P),_Storia,avanzo(S1),st(S1,P)) :-
	game_area(S1),  % indica che non contiene ostacoli né npc
	retract(soldato(S0)),
	assert(soldato(S1)),
	avanza_tempo.  % predicato che incrementa il valore dell'orologio
esegui_azione(st(S0,P),_Storia,aspetto,st(S0,P)) :-
	avanza_tempo.  % cambia solo il valore dell'orologio


/**** PIANIFICAZIONI ****/
piano(_Stato,_Storia,vado(S,P),Piano) :-
	cerca_un_piano(S,P,Piano).

pred cerca_un_piano(punto,punto,list(decisione)).
%%	cerca_un_piano(+S,+P,-DecList) SEMIDET
%%	Spec: vero sse DecList è una lista di decisioni (azioni) che
%	porta da S a P. Il predicato fallisce se non esiste una lista di
%	decisioni possibili.

pred current_goal(punto).
%%	current_goal(?G) SEMIDET
%%	Spec: vero sse G è il goal da usare nella libreria mr.pl
:-dynamic(current_goal/1).

cerca_un_piano(P,G,Piano) :-
	retractall(current_goal(_)),
	assert(current_goal(G)),
	solve(P,Soluzione,=(G)),
	estrai_piano(Soluzione,Piano).

% NOTA: leggere bene la libreria di ricerca per sfruttarla al meglio
type [nc(punto,list(punto),number)]:nodo.
% importo tipo nodo per search

pred estrai_piano(nodo, list(decisione)).
%%	estrai_piano(+Sol,-Piano) DET
%%	Spec: vero sse Piano è la sequenza di avanzamenti che percorre
%	la sequenza di posizioni calcolata nella soluzione Sol
estrai_piano(nc(G,RevPath,_C),Piano) :-
	reverse([G|RevPath],[_Start|Path]),
	path2moves(Path,Piano).

pred path2moves(list(punto),list(decisione)).
%%	path2moves(+PList,-DList) DET
%%	Spec: vero sse DList è una lista di decisioni che rappresentano
%	il percorso di PList
path2moves([P|Path],[avanzo(P)|MovList]) :-
	path2moves(Path,MovList).
path2moves([P,P|Path],[aspetto|MovList]) :-
	path2moves(Path,MovList).
pathmoves([],[]).
