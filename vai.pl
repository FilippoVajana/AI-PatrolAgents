:- use_module(library(is_a)).
:- use_module(vai_spec).
:- use_module(vai_if).
%  importo tipi e predicati dai moduli di specifica e di interfaccia
type T :- vai_spec:type(T).
type T :- vai_if:type(T).
pred P :- vai_spec:pred(P).
pred P :- vai_if:pred(P).
ignored P :- vai_spec:ignored(P).


/************************************************************
 *  A) IL CICLO DI DECISIONE-PIANIFICAZIONE-ESECUZIONE      *
 ************************************************************/

start(Avvio) :-
	%  COMANDO DI AVVIO DELLA STORIA
	clear_counts,                  % contatori di debug azzerati
	stato_iniziale(S0,Avvio),      % stato iniziale dal progetto
	ricorda(S0,[],inizio_storia(Avvio),Storia0),
	mostra_start(S0),
	vai(S0, Storia0).	% avvio storia

vai(Stato, Storia) :-
     decidi(Stato, Storia, Decisione),
     (	fine(Decisione),!,
	% l'agente ha deciso di finire
	mostra_fine(Stato,Decisione,Storia)
	;

	% l'agente non ha deciso di finire e pianifica e attua la decisione
	trace_debug(mostra_decisione(Stato, Storia, Decisione)),
	(
	pianifica(Stato, Storia, Decisione, Piano) ->
	%  vi � un piano di attuazione della Decisione;
	%  per debugging si mostra il piano
	%trace_debug(mostra_piano(Stato, Storia, Decisione, Piano)),
	ricorda(Stato, Storia, pianificata(Decisione,Piano), PStoria),
	esegui(Stato, PStoria, Decisione, Piano, NStato, NStoria)
	;

	%  non vi � un piano di attuazione della decisione;
	%  l'agente non cambia stato e ricorda l'impossibilita'
	NStato=Stato,
	ricorda(Stato, Storia, impossibile(Decisione), NStoria)
	), !,

	% e la storia prosegue
	vai(NStato, NStoria)
    ).

esegui(Stato,Storia, Decisione, [A|Piano], NStato, NStoria) :-
	esegui_azione(Stato, Storia, A, AStato), !,
	%  l'azione A del piano in esecuzione � stata eseguita con
	%  successo, l'agente ricorda l'avvenuta transizione di stato
	ricorda(Stato, Storia, transizione(Stato, A, AStato), AStoria),
	mostra_transizione(Stato, A, AStato),
	esegui(AStato,AStoria,Decisione,Piano,NStato,NStoria).
esegui(Stato,Storia,Decisione, [A|Piano], Stato, NStoria) :-
	% se arrivo qui, per il cut l'esecuzione del piano per la
	% Decisione corrente � fallita in corrispondenza dell'azione A
	% (che � fallita) e l'agente lo ricorda
	ricorda(Stato, Storia, fallita(Decisione, [A|Piano]), NStoria).
esegui(Stato,Storia,Decisione,[],Stato,NStoria) :-
	% tutte le azioni del piano relativo a Decisione sono state
	% eseguite, l'agente lo ricorda
	ricorda(Stato, Storia, eseguita(Decisione), NStoria).


% NOTA BENE. da riscrivere se opportuno per adattarle al progetto

ricorda(Stato, Storia,Evento, [Evento|Storia]) :-
	%  quando l'agente ricorda un evento aggiorna anche
	%  la base di conoscenza dinamica
	aggiorna_conoscenza(Stato,Storia,Evento),

	%  gli eventi sono tracciati per il debugging
	trace_debug(ricorda(Stato, Storia,Evento,[Evento|Storia])).

pianifica(_S,_Storia,Decisione,[Decisione]) :-
	azione(Decisione), !.
pianifica(S,Storia,Decisione,Piano) :-
	piano(S,Storia, Decisione,Piano).

/**********************************************************
 *   B)  GESTIONE DELLA CONOSCENZA E METAINTERPRETE       *
 **********************************************************/

:- dynamic(conosce/1).
:- dynamic(assunto/1).

%%%   B1) Acquisizione nuova conoscenza e revisione della vecchia

impara(A) :-
	not(ground(A)),!,
	throw(error(impara(A), 'NON GROUND, PUOI IMPARARE SOLO COSE GROUND')).
impara(A) :-
       forall(assunto(Ass), (inconsistenti(Ass,A) -> retract(assunto(Ass)) ; true)),
       assert(conosce(A)).

inconsistenti(A1,A2) :-
	A1=non(A2), !
	;
	A2=non(A1), !
	;
	contraria(A1,A2).

%%%   B2) Metainterprete per un agente che "pensa" con assunzioni ed
%%%   usa la base di conoscenza dinamica ed eventuali Info addizionali

pensa(_Info, true, Ass, Ass) :- !.
pensa(Info, (A,B), Ass1, Ass2) :- !,
	pensa(Info, A,Ass1, Ass),
	pensa(Info, B,Ass,Ass2).
pensa(Info, (A;B), Ass1, Ass2) :- !,
	pensa(Info, A,Ass1, Ass2);
	pensa(Info, B,Ass1,Ass2).
pensa(Info, A,Ass1,Ass2) :-
	assumibile(A),!,
	assume_o_conosce(Info, A,Ass1,Ass2).
pensa(Info, A,Ass1,Ass2) :-
	meta(A),!,
	clause(A,Body),
	pensa(Info, Body,Ass1,Ass2).
pensa(_Info, A,Ass,Ass) :-
	call(A).

assume_o_conosce(_Info, A, Ass,Ass) :-
       conosce(A), !.
assume_o_conosce(_Info, A, Ass,[A|Ass]) :-
       assunto(A), !.
assume_o_conosce(Info, non(A), Ass, [non(A)|Ass]) :- !,
      not(conosce(A)),
      decide_se_assumere(Info,non(A)),
      check_ground(non(A)).
assume_o_conosce(Info, A, Ass, [A|Ass]) :-
      not(conosce(non(A))),
      decide_se_assumere(Info, A),
      check_ground(A).

check_ground(A) :-
	  not(ground(A)) ->
	  throw(error(assume_o_conosce(A),
		      'NON GROUND, PUOI ASSUMERE SOLO COSE GROUND'))
	; true.

%  B2.1)  AGENTE SEMPLIFICATO: quando pensa e decide_se_assumere non usa
%  Info addizionali
pensa(A,Ass) :-
	pensa([], A,[],Ass).
decide_se_assumere([], A) :-
	decide_se_assumere(A).

%%% B3)  L'agente sa una cosa se la pensa senza assumere nulla

sa(Info,A):-
	pensa(Info, A,[],[]).
sa(A) :-
	pensa([], A,[],[]).

/********************************************************
 *	C)  PER DEBUGGING DEL PROGETTO			*
 ********************************************************/

:- dynamic(debug_on/0).
attiva_debug :-
	debug_on, !; assert(debug_on).
disattiva_debug :-
	noshow,
	retractall(debug_on).

clear_counts :- nb_setval(debug_c,0).
inc_debug_c :-
	nb_getval(debug_c,C),
	C1 is C+1,
	nb_setval(debug_c,C1).

trace_debug(_) :-
	not(debug_on), !.
trace_debug(ricorda(Stato,Storia,Evento,_NStoria)) :-
	catch(mostra_stato(Stato),_,d_mostra_stato(Stato)),
	catch(mostra_evento(Evento),_,d_mostra_evento(Evento)),
	catch(mostra_storia(Storia),_, d_mostra_storia(Storia)),
	catch(mostra_conoscenza,_, d_mostra_conoscenza),
	inc_debug_c,
        ask.
trace_debug(mostra_decisione(St, Storia, Decisione)) :-
        catch(mostra_decisione(St, Storia, Decisione),_,
	      d_mostra_decisione(St, Storia, Decisione)),
	ask.
trace_debug(stop(St,Storia)) :-
	catch(stop(St,Storia),_,
	      d_stop(St,Storia)).


mostra_fine(ST,Decisione, Storia) :-
       catch(stop(ST,Decisione,Storia),_, d_stop(ST,Decisione, Storia)).

ask(Msg) :-
	atomic_concat(Msg,': ',P),
	prompt1(P),
	readln(R),
	command(R).

ask :-
	nb_getval(debug_c,C),
	atomic_concat(C,': ',P),
	prompt1(P),
	readln(R),
	command(R).

command([]).
command([S|_]) :-
	(   S=a -> abort
	;   S=t -> trace
	;   S=s -> show
	;   S=n -> noshow
	;    true).


/***********************  APPENDICE ****************************
*       VISUALIZZAZIONI DI DEFAULT USATE		       *
*       IN ASSENZA DI QUELLE PERSONALIZZATE                    *
****************************************************************/

/************* SE volete una diversa "animazione" dovete definire
 * mostra_stato_iniziale(S), mostra_transizione_stato(S1,A,S2),
 * (mostra_conoscenza,
 * oppure sostituire per inytero mostra_start e mostra_transizione
 * ***************************************************************/

mostra_start(S) :-
	not(debug_on),!,
	catch(mostra_stato_iniziale(S), _, writeln('START nello stato':S)),
	catch(mostra_conoscenza,_,d_mostra_conoscenza),
	catch(mostra_conoscenza,_,d_mostra_conoscenza),
	ask('<RETURN> per proseguire').
mostra_start(_).

mostra_transizione(S1,A,S2) :-
	not(debug_on),!,
	catch(mostra_transizione_stato(S1,A,S2),_,
	      writeln('ESEGUITA ':S1->A->S2)),
	catch(mostra_conoscenza,_,d_mostra_conoscenza),
	ask('<RETURN> per proseguire').
mostra_transizione(_S1,_A,_S2).


% da sostituire se si vuole una diversa visualizzazione dello
% statosia in fase di "animazione", sia in fase di debug; in progetto.pl
% � stata sostituita mostrando la mappa dello stato di conoscenza
% dell'agente
d_mostra_stato(S) :-
	maplist(write, ['Stato:      ', S, '\n']).

%  le seguenti sono da sostituire per mostrare gli eventi, la storia,
%  ecc. in modo diverso in fase di debug
d_mostra_evento(S) :-
	maplist(write, ['Evento:     ', S, '\n']).
d_mostra_storia([]) :-
	writeln('Storia:     []').
d_mostra_storia([E|_R]) :-
	maplist(write, ['Storia:   ', [E|_], '\n']).
d_mostra_decisione(St, _, Decisione) :-
	maplist(write, ['Stato : ', St, ', decisione: ',
			Decisione, '\n    pianifico']).
d_mostra_conoscenza :-
	write('Conosce:    '),
	forall(conosce(A), (writeq(A), write(','))), nl,
	write('Assunzioni: '),
	forall(assunto(A), (writeq(A), write(','))), nl.
d_stop(Stato,Decisione, Storia) :-
	(   debug_on ->
	    writeln('*************  FINE  ***********'),
	    d_mostra_stato(Stato)
	;   true
	),
	write('\n\nDECISO DI FINIRE ':Decisione),
	write('\n\nPer vedere tutta la storia rispondi s-i: '),
	readln(Risp),
	(   Risp=[s|_] ->
	    reverse(Storia, RStoria),
	    maplist(writeln, RStoria)
	;   true
	).




