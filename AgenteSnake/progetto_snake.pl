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

%%	NOTA: meglio definire tempo in livello.pl
type [{integer}]: tempo.

/***   IMPLEMENTO I TIPI DI vai_if ******/

gen [mappa(number)]: info_inizio.
	% mappa(I):  identifica la mappa caricabile n. I

gen [st(punto, punto, tempo)]:stato.
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

gen [ronda(sentinella,punto,direzione,tempo)]: assumibile.
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



/**** IMPLEMENTAZIONE DEI PREDICATI RICHIESTI DA SEARCH_IF.PL ****/
trovato(st(P,P,_)).
%%	La ricerca si ferma quando la posizione del soldato coincide con
%	quella del prigioniero.

vicini(st(Soldato1,Prigioniero,Tempo),ListaVicini) :-
	adiacenti(Soldato1,ListaAdiacenti),
	TempoVicino is Tempo + 1,
	points2states(ListaAdiacenti,Prigioniero,TempoVicino,ListaVicini),

pred elimina_non_validi(list(punto),list(punto)).
%%	elimina_non_validi(+ListaPunti,+NuovaLista) DET
%%	Spec: vero sse NuovaLista contiene solo i punti di ListaPunti a
%	cui corrisponde un'area percorribile.

pred converti_lista(list(punto),punto,tempo,list(stato)).
%%	converti_lista(+ListaPunti,+Prigioniero,+TempoVicino,-ListaStati)
%	DET Spec: vero sse ListaStati rappresenta gli elementi di
%	ListaPunti convertiti in stati. Prigioniero è la posizione del
%	prigioniero e serve a costruire gli stati. Stessa cosa per
%	TempoVicino che è l'orario del nodo vicino.

elimina_non_validi([],[]) :- !.
elimina_non_validi([Testa | Coda],[Testa | NuovaLista]) :-
	game_area(Testa), !,
	elimina_non_validi(Coda,NuovaLista).
elimina_non_validi([_Testa | Coda],NuovaLista) :-
	elimina_non_validi(Coda,NuovaLista).

converti_lista([],_,_,[]) :- !.
converti_lista([S | Coda],P,TempoVicino,[st(S,P,TempoVicino) | StatiCoda]) :-
	converti_lista(Coda,P,TempoVicino,StatiCoda).

pred points2states(list(punto),punto,tempo,list(stato)).
%%	points2states(+ListaPunti,+Prigioniero,+TempoVicino,-ListaStati)
%	DET
%%      Spec: vero sse unisce le chiamate a elimina_non_validi e
%	converti_lista in un solo predicato.
points2states(Punti,Prigioniero,TempoVicino,Stati) :-
	elimina_non_validi(Punti,NuoviPunti),
	converti_lista(NuoviPunti,Prigioniero,TempoVicino,Stati).

costo(st(Soldato,Prigioniero),st(Soldato2,Prigioniero),2) :-
	Soldato \= Soldato2, !.
costo(st(Soldato,Prigioniero),st(Soldato,Prigioniero),1).
%%	Spostarsi costa 2, aspettare costa 1.


h(st(Soldato,Prigioniero,_Orario),H) :-
	distanza_euclidea(Soldato,Prigioniero,H).
%%	L'euristica usata è per ora la distanza euclidea tra Soldato e
%	Prigioniero.


/**** LA TERRIBILE PARTE DI RAGIONAMENTO ****/

aggiorna_conoscenza(st(S,P,T),_H,inizio_storia(_Avvio)) :-
	% in teoria a inizio storia l'agente dovrebbe assumere i percorsi delle
	% sentinelle
	pensa(/* non so cosa mettere :-) */).
aggiorna_conoscenza(st(S,P,T),_H,transizione(S1,Dec,S2)) :-
	% il tempo è avanzato
	clock(Ora),
	assunto(ronda(Sentinella,PosizioneAssunta,DirezioneAssunta,Ora)),
	sentinella(Sentinella,PosizioneEffettiva,DirezioneEffettiva),
	impara(ronda(Sentinella,PosizioneEffettiva,DirezioneEffettiva,Ora)),
	% se ha sbagliato cancella le assunzioni e ne fa di nuove
	not(PosizioneAssunta is PosizioneEffettiva,
	    DirezioneAssunta is DirezioneEffettiva) ->
	        bagof(O,(assunto(ronda(Sentinella,_,_,O)),O >= Ora),O2),
	        forall(member(O3,O2),retract(assunto(ronda(Sentinella,_,_,O3)))).
% NOTA: indagare meglio su questo predicato

assumibile(ronda(_,_,_,_)).
% Il soldato può ipotizzare una ronda per le sentinelle

contraria(ronda(Sentinella,Pos,Dir,Ora),ronda(Sentinella,PosDiv,DirDiv,Ora)) :-
	Pos \= PosDiv;
	Dir \= DirDiv.
% una sentinella non può trovarsi in due punti nello stesso momento
contraria(ronda(Sentinella,P1,_D1,Ora),ronda(Sentinella,P2,_D2,OraSucc)) :-
	not(OraSucc is Ora + 1);
	not(next(P1,P2)).
