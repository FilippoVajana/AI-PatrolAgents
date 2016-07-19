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

% IMPORTO MODULI SPECIFICI DEL PROGETTO
:- use_module('sentinella').



type T :- vai_spec:type(T).
type T :- vai_if:type(T).
type T :- livello_spec:type(T).
type T :- sentinella:type(T).
pred P :- vai_spec:pred(P).
pred P :- vai_if:pred(P).
pred P :- livello_spec:pred(P).
pred P :- sentinella:pred(P).

%%	NOTA: meglio definire tempo in livello.pl
type [{integer}]: tempo.

/***   IMPLEMENTO I TIPI DI vai_if ******/

gen [mappa(number)]: info_inizio.
	% mappa(I):  identifica la mappa caricabile n. I

gen [st(punto, punto, tempo)]:stato.
    % st(P,G): il soldato si trova in P e deve andare a G
    % NOTA: possibili aggiunte di termini allo stato
         % tempo: perche' le posizioni dei nemici cambiano a seconda del tempo
         % visibilita' : da usare se si implementa la possibilitÃ  di nascondersi

/******   Le decisioni dell'agente  */
gen [ % piani
     vado(punto, punto),
       % decisione vado(P,G): cerco un piano per andare da P a G
     attesa(integer),
       % decido di aspettare un certo tempo
     % azioni
     aspetto,
       % azione aspetto: sto fermo per un turno
     avanzo(punto),
       % azione avanzo(P): avanzo nel punto indicato
     termino(evento)
    ]: decisione.

gen [step_ronda(sentinella,punto,direzione,tempo)]: assumibile.
% NOTA: forse anche guardia/2 dovrebbe essere assumibile, ma in teoria basta
%  assumere il pattern

pred clear_knowledge.
% comando qui introdotto per azzerare la conoscenza, che altrimenti
% resta acquisita ad ogni esecuzione e si accumula

clear_knowledge :-
	retractall(assunto(_)),
	retractall(conosce(_)).

% specificato in vai_if; l'informazione iniziale ÃƒÂ¨ qui
% la mappa che vogliamo usare; se giÃƒÂ  caricata non viene
% azzerata conoscenza dinamica dell'agente, che di volta in volta
% impara; se si passa a nuova mappa, l'agente parte con
% conoscenza dinamica nulla;  usiamo  ultima/1 per ricordare
% l'ultima mappa caricata
:- dynamic(ultima/1).

stato_iniziale(st(S0,P,ClockIniziale), mappa(I)) :-
	strategy(astar),
	strategy(pota_chiusi),
	carica_mappa(I),
	position(S0),
	goal(P),
	azzera_clock,
	clock(ClockIniziale),
	(   ultima(I) ->
	    writeln('CONTINUAZIONE SU ':mappa(I))
	;   retractall(ultima(_)),
	    clear_knowledge,
	    writeln('NUOVA ':mappa(I)),
	    assert(ultima(I))
	).

/****  B: le decisioni *******/

% fine e' specificato in vai_if; la decisione finale e' termino(...)
fine(termino(_)).

% decidi e' specificato in vai_if;  lo implemento come segue
% 1) sono a inizio storia con stato st(S,P,T);
decidi(st(Soldato,Prigioniero,_Tempo),
       [inizio_storia(_I)],
       % decido di cercare un piano per andare da P0 a G
       vado(Soldato,Prigioniero)).

% 2) ho eseguito la decisione precedente _Dec;
decidi(st(Soldato,Prigioniero,_Tempo),
       [eseguita(Dec)|_],
       Decisione)
:- Soldato = Prigioniero ->
   % se la mia posizione e' il goal, termino
   Decisione is termino(eseguita(Dec))
   ;
   %  altrimenti cerco un piano per andare da Soldato a Prigioniero
   Decisione is vado(Soldato, Prigioniero).

% 3) nella ricerca del piano ho verificato che il prigioniero e'
% irraggiungibile oppure non si puo' raggiungere evitando le guardie;
decidi(_ST,
       [impossibile(vado(Soldato,Prigioniero))|_],
       % termino per impossibilita'  di raggiungere il goal
       termino(impossibile(vado(Soldato,Prigioniero)))).

% 4A) sono stato visto da una guardia mentre tentavo di raggiungiere il
% prigioniero;
decidi(Stato,
       [fallita(vado(S,P),[avanzo(_)|_])|_],
       termino(fallita(vado(S,P))))
:- avvistato(Stato,_Sentinella).
% 4B) sono stato visto da una guardia mentre aspettavo in un punto.
decidi(Stato,
       [fallita(vado(S,P),[aspetto|_])|_],
       termino(fallita(vado(S,P))))
:- avvistato(Stato,_Sentinella).

%%	NOTA: da implementare
pred avvistato(stato,id_sentinella).
%%	avvistato(?Stato,-Sentinella) SEMIDET
%%	Spec: vero sse il giocatore viene avvistato da Sentinella quando
%	si trova nello stato specificato.

avvistato(st(_S,_P,_T),Sentinella) :-
	soldato_avvistato(Sentinella).

/*
pred avvistato(stato, punto, sentinella).
%%	avvistato(-PosizioneSoldato,-PosizioneSentinella,-Nome) SEMIDET
%%	Spec: vero sse Nome e' la sentinella in PosizioneSentinella che
%	ha avvistato l'agente in PosizioneSoldato

avvistato(p(SX,SY),p(SENTX,SENTY),NAME) :-
  sentinella_dove(p(SENTX,SENTY),_,NAME), %% NOTA da implementare. L'implementazione in livello.pl contiene parametri non usati
  area_sentinella(p(SENTX,SENTY),NAME, area(p(X1,Y1),p(X2,Y2))), % NOTA: la specifica chiede due parametri, l'implementazione ne da quattro. In teoria dovrebbero bastarne tre (coordinate, direzione, nome)
  punto_area(p(SX,SY),area(p(X1,Y1),p(X2,Y2))).
*/
/**** AZIONI ****/

azione(avanzo(_)).
azione(aspetto).
azione(termino(_)).

% Specifica in vai_if.pl
esegui_azione(st(S0,P,T),_Storia,avanzo(S1),st(S1,P,TNext)) :-
	game_area(S1),  % indica che non contiene ostacoli né npc [DA DEFINIRE]
	retract(soldato(S0)),
	assert(soldato(S1)),
	clock(T),
	aggiorna_clock,  % predicato che incrementa il valore dell'orologio
	clock(TNext).
esegui_azione(st(S0,P,T),_Storia,aspetto,st(S0,P,TNext)) :-
	clock(T),
	aggiorna_clock,  % cambia solo il valore dell'orologio
        clock(TNext).


/**** PIANIFICAZIONI ****/
piano(_Stato,_Storia,vado(S,P),Piano) :-
	cerca_un_piano(S,P,Piano).

pred cerca_un_piano(punto,punto,list(decisione)).
%%	cerca_un_piano(+S,+P,-DecList) SEMIDET
%%	Spec: vero sse DecList e' una lista di decisioni (azioni) che
%	porta da S a P. Il predicato fallisce se non esiste una lista di
%	decisioni possibili.

pred current_goal(punto).
%%	current_goal(?G) SEMIDET
%%	Spec: vero sse G e' il goal da usare nella libreria mr.pl
:-dynamic(current_goal/1).

cerca_un_piano(P,G,Piano) :-
	retractall(current_goal(_)),
	assert(current_goal(G)),
	solve(P,Soluzione,=(G)),
	estrai_piano(Soluzione,Piano).


type [nc(punto,list(punto),number)]:nodo.
% importo tipo nodo per search

pred estrai_piano(nodo, list(decisione)).
%%	estrai_piano(+Sol,-Piano) DET
%%	Spec: vero sse Piano Ã¨ la sequenza di avanzamenti che percorre
%	la sequenza di posizioni calcolata nella soluzione Sol
estrai_piano(nc(G,RevPath,_C),Piano) :-
	reverse([G|RevPath],[_Start|Path]),
	path2moves(Path,Piano).

pred path2moves(list(punto),list(decisione)).
%%	path2moves(+PList,-DList) DET
%%	Spec: vero sse DList e' una lista di decisioni che rappresentano
%	il percorso di PList
path2moves([],[]) :- !.
path2moves([P,P|Path],[aspetto|MovList]) :-
	path2moves(Path,MovList), !.
path2moves([P|Path],[avanzo(P)|MovList]) :-
	path2moves(Path,MovList).



/**** IMPLEMENTAZIONE DEI PREDICATI RICHIESTI DA SEARCH_IF.PL ****/
trovato(st(P,P,_)).
%%	La ricerca si ferma quando la posizione del soldato coincide con
%	quella del prigioniero.

vicini(st(Soldato,Prigioniero,Tempo),ListaVicini) :-
	adiacenti(Soldato,ListaAdiacenti),
	TempoVicino is Tempo + 1,
	points2states(ListaAdiacenti,Prigioniero,TempoVicino,
		      [Soldato | ListaVicini]).

pred pensa_avvistato(stato,id_sentinella).
%%	pensa_avvistato(+St,-Se) SEMIDET
%%	Spec: vero sse l'agente nello stato St, secondo le sue
%	assunzioni, crede che sarà visto dalla sentinella Se
%	nell'istante tempo definito dal suo stato.

pensa_avvistato(st(Soldato,_P,Tempo),Sentinella) :-
	pensa(step_ronda(Sentinella,PosizioneSentinella,_,Tempo),_),
	area_sentinella(PosizioneSentinella,Area), %% area_sentinella non dovrebbe tener conto della sua direzione?
	punto_area(Soldato,Area).


pred elimina_non_validi(list(punto),list(punto)).
%%	elimina_non_validi(+ListaPunti,-NuovaLista) DET
%%	Spec: vero sse NuovaLista contiene solo i punti di ListaPunti a
%	cui corrisponde un'area percorribile e in cui l'agente non pensa
%	di poter essere avvistato.

pred converti_lista(list(punto),punto,tempo,list(stato)).
%%	converti_lista(+ListaPunti,+Prigioniero,+TempoVicino,-ListaStati)
%	DET Spec: vero sse ListaStati rappresenta gli elementi di
%	ListaPunti convertiti in stati. Prigioniero è la posizione del
%	prigioniero e serve a costruire gli stati. Stessa cosa per
%	TempoVicino che è l'orario del nodo vicino.

pred elimina_avvistati(list(stato),list(stato)).
%%	elimina_avvistati(+ListaStati,-ListaStatiSicuri) DET
%%	Spec: vero sse elimina_avvistati elimina da ListaStati tutti gli
%	stati in cui l'agente pensa di venire avvistato.

pred points2states(list(punto),punto,tempo,list(stato)).
%%	points2states(+ListaPunti,+Prigioniero,+Tempo,-ListaStati) DET
%%	Spec: vero sse unisce le chiamate a elimina_non_validi,
%%	converti_lista e elimina_avvistati in un solo predicato.


elimina_non_validi([],[]) :- !.
elimina_non_validi([Testa | Coda],[Testa | NuovaLista]) :-
	game_area(Testa), !,
	elimina_non_validi(Coda,NuovaLista).
elimina_non_validi([_Testa | Coda],NuovaLista) :-
	elimina_non_validi(Coda,NuovaLista).


converti_lista([],_,_,[]) :- !.
converti_lista([S | Coda],P,TempoVicino,[st(S,P,TempoVicino) | StatiCoda]) :-
	converti_lista(Coda,P,TempoVicino,StatiCoda).


elimina_avvistati([],[]) :- !.
elimina_avvistati([Testa | Coda], [Testa | NuovaLista]) :-
	not(pensa_avvistato(Testa,_)), !,
	elimina_avvistati(Coda,NuovaLista).
elimina_avvistati([_ | Coda], NuovaLista) :-
	elimina_avvistati(Coda,NuovaLista).


points2states(Punti,Prigioniero,TempoVicino,StatiFinali) :-
	elimina_non_validi(Punti,NuoviPunti),
	converti_lista(NuoviPunti,Prigioniero,TempoVicino,Stati),
	elimina_avvistati(Stati,StatiFinali).



costo(st(Soldato,Prigioniero,Tempo),st(Soldato2,Prigioniero,TempoNext),2) :-
	Soldato \= Soldato2,
	Tempo = TempoNext - 1, !. % NOTA: controllare uso di = o is
costo(st(Soldato,Prigioniero,Tempo),st(Soldato,Prigioniero,TempoNext),1) :-
	Tempo = TempoNext - 1.
%%	Spostarsi costa 2, aspettare costa 1.


h(st(Soldato,Prigioniero,_Orario),H) :-
	distanza_euclidea(Soldato,Prigioniero,H).
%%	L'euristica usata è per ora la distanza euclidea tra Soldato e
%	Prigioniero.




/**** LA TERRIBILE PARTE DI RAGIONAMENTO ****/

pred posizione_iniziale_sentinella(sentinella, punto, direzione).
%%	posizione_iniziale_sentinella(?S,?P,?D) SEMIDET
%%	Spec: vero sse all'istante 0 S si trova in P e guarda verso D.
%	Usato solo dall'agente per fare assunzioni sulla ronda.
meta(posizione_iniziale_sentinella(_,_,_)).
meta(avvistato(_,_)).
:-dynamic(posizione_iniziale_sentinella/3).

pred step_ronda(sentinella, punto, direzione, tempo).
%%	step_ronda(?S,?P,?D,?T) SEMIDET
%%	Spec: vero sse all'istante T S si trova in P e guarda verso D.
%	Usato solo dall'agente per fare assunzioni sulla ronda.

aggiorna_conoscenza(st(_S,_P,_T),_H,inizio_storia(_Avvio)) :-
	% a inizio storia il soldato deve memorizzare le posizioni iniziali delle           sentinelle
	retractall(conosce(posizione_iniziale_sentinella(_,_,_))),
	stato_sentinella(Sentinella, Posizione, Direzione),
	impara(posizione_iniziale_sentinella(Sentinella, Posizione, Direzione)),
	impara(step_ronda(Sentinella, Posizione, Direzione)).
aggiorna_conoscenza(st(_S,_P,T),_H,transizione(_S1,_Dec,_S2)) :-
	% l'ora attuale e' quella dello stato (T)
	stato_sentinella(Sentinella,Posizione,Direzione),
	impara(step_ronda(Sentinella,Posizione,Direzione,T)).


assumibile(step_ronda(_,_,_,_)).
% Il soldato può ipotizzare una ronda per le sentinelle

% 1) L'agente ha notato che una sentinella si trova in un punto in cui
% e' gia' passata, quindi da questo momento l'agente assume che si
% posizioni sempre in quel punto a intervalli regolari.
decide_se_assumere(step_ronda(S,P,D,T)) :-
	clock(Ora),
	stato_sentinella(S,P,D),
	trovato_loop(S,P,D,Ora,UltimaVolta), !,
	Durata is Ora - UltimaVolta,
	estrai_passi(S,UltimaVolta,Ora,Durata,ListaPassi),
	member(step_ronda(S,P,D,T),ListaPassi).
% 2) L'agente non ha notato dei loop e quindi assume che la sentinella
% compia un percorso rettilineo a partire dalla sua posizione iniziale.
decide_se_assumere(step_ronda(S,P,D,T)) :-
	posizione_iniziale_sentinella(S,P0,D0),
	lunghezza_percorso_rettilineo(LunghezzaPercorso),
	percorso_rettilineo(S,P0,D0,0,LunghezzaPercorso,Lista),
	member(step_ronda(S,P,D,T),Lista).

% NOTA: Siamo sicuri che non sia corretto assumere piu' posizioni
% simultanee per una sentinella? Assumere piu' posizioni potrebbe
% rendere il percorso piu' sicuro. Per ora commento
/*
contraria(step_ronda(Sentinella,Pos,Dir,Ora),step_ronda(Sentinella,PosDiv,DirDiv,Ora)) :-
	Pos \= PosDiv;
	Dir \= DirDiv.
*/
% una sentinella non può trovarsi in due punti nello stesso momento
contraria(step_ronda(Sentinella,P1,_D1,Ora),step_ronda(Sentinella,P2,_D2,OraSucc)) :-
	OraSucc == Ora;
	not(next(P1,P2)).

pred trovato_loop(sentinella, punto, direzione, tempo, tempo).
%%	trovato_loop(+S,+P,+D,+Ora,-UltimaVolta) SEMIDET
%%	Spec: vero sse S all'istanta Ora si trova in P,D e si trovava
%	nella stessa posizione anche all'istante UltimaVolta.

trovato_loop(S,P,D,Ora,UltimaVolta) :-
	clock(Ora),
	% bisogna assicurarsi che il passo identico trovato sia il piu' recente
	bagof(T,(conosce(step_ronda(S,P,D,T)),T > Ora),ListaPassiPrecedenti),
	max_member(UltimaVolta,ListaPassiPrecedenti),
	conosce(step_ronda(S,P,D,UltimaVolta)).

pred estrai_passi(sentinella, tempo, tempo, integer, list(step_ronda(_,_,_,_))).
%%	estrai_passi(+S,+UltimaVolta,+Ora,+Durata,-ListaPassi) DET
%%	Spec: Prende tutti gli step eseguiti dall'istante UltimaVolta
%	all'istante Ora e produce una nuova serie di passi identici che
%	si sviluppano a partire dall'istante Ora. Durata indica il
%	numero di step che compongono la ronda

estrai_passi(S,Ora,Ora,Durata,[step_ronda(S,P,D,T)]):-
	conosce(step_ronda(S,P,D,Ora)),
	T is Ora + Durata.
estrai_passi(S,UltimaVolta,Ora,Durata,[step_ronda(S,P,D,T) | Coda]) :-
	UltimaVolta =< Ora,
	conosce(step_ronda(S,P,D,UltimaVolta)),
	T is UltimaVolta + Durata,
	Next is UltimaVolta + 1,
	estrai_passi(S,Next,Ora,Durata,Coda).

pred percorso_rettilineo(sentinella,posizione,direzione,tempo,integer,list(step_ronda(_,_,_,_))).
%%	percorso_rettilineo(+S,+P,+D,+T,+Durata,-ListaPassi) DET
%%	Spec: produce un percorso rettilineo (andata e ritorno) di un
%	numero di caselle pari a Durata, che inizia al tempo T alla
%	posizione P,D, per la sentinella S.

percorso_rettilineo(S,P,D,T,Durata,ListaFinale) :-
	linea_retta(S,P,D,T,Durata,ListaAndata),
	last(ListaAndata,step_ronda(S,P1,D1,T1)),
	direzione_opposta(D1,Dopp),
	linea_retta(S,P1,Dopp,T1,Durata,ListaRitorno),
	append(ListaAndata,ListaRitorno,ListaFinale).

pred linea_retta(sentinella,punto,direzione,tempo,integer,list(step_ronda(_,_,_,_))).
%%	linea_retta(+S,+P,+Dir,+TStart,+Lun,-List) DET
%%	Spec: vero sse List e' una lista di passi che compongono un
%	percorso rettilineo per S che comincia in P al tempo TStart e
%	avanza per Lun passi in direzione Dir.

pred linea_retta(sentinella,punto,direzione,tempo,integer,integer,list(step_ronda(_,_,_,_))).
%%	linea_retta(+S,+P,+Dir,+TStart,+Lun,++Calc-List) DET
%%	Spec: come sopra, usato internamente da Prolog per i calcoli.

linea_retta(S,Pos,Dir,TStart,Lun,List) :-
	linea_retta(S,Pos,Dir,TStart,Lun,0,List).
linea_retta(S,PStart,Dir,TStart,Durata,Calcolati,[step_ronda(S,P,Dir,T)|Coda]) :-
	Calcolati < Durata, !,
	passo_avanti(PStart,Dir,P), % NOTA: implementare questa cosa!
	T is TStart + Calcolati,
	NewCalcolati is Calcolati + 1,
	linea_retta(S,P,Dir,TStart,Durata,NewCalcolati,Coda).
linea_retta(_,_,_,_,C,C,[]).

pred lunghezza_percorso_rettilineo(integer).
%%	lunghezza_percorso_rettilineo(-Lun) DET
%%	Spec: predicato che serve a impostare la lunghezza del percorso
%	rettilineo su cui l'agente fa assunzioni
:-dynamic(lunghezza_percorso_rettilineo/1).
lunghezza_percorso_rettilineo(3).











