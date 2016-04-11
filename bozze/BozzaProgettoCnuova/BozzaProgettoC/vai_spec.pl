:- module(vai_spec, []).

:- use_module(library(is_a)).
:- discontiguous(pred(_)).
:- discontiguous(type(_)).

/*****************  SPECIFICA ****************************/

type  [[],[T|list(T)]]:list(T).
type [
      %%  INIZIANO DECISIONI
      impossibile(decisione),
         % impossibile(Dec):  Dec non ha un piano d'esecuzione
      eseguita(decisione),
	 % eseguita(Dec):  eseguita completamente la decisione Dec
      fallita(decisione, list(decisione)),
         % fallita(Dec, [A|..]): piano per Dec interrotto in A
      inizio_storia(info_inizio),
         % evento d'inizio con informazioni iniziali

      %% EVENTI INTERMEDI, NON INIZIANO DECISIONI
      deciso(decisione),
         %deciso(Dec):  presa la decisione Dec
      pianificata(decisione, list(decisione)),
         % pianificata(Dec, Piano):  Piano è un piano d'esec. per Dec
      transizione(stato, decisione, stato),
         % transizione(S1,A,S2):  eseguita l'azione S1 -A-> S2,
      stop(stato,list(evento))
     ]: evento.
type [ {assumibile}, non(assunzione)]: assunzione.

/******  PREDICATI IMPLEMENTATI DA vai e usabili nel progetto ****/

%%	A)  COMPORTAMENTO

pred start(info_inizio).
%  start(+I): comando che inizializza i contatori per il debugging, usa
%  stato_iniziale/2 per estrarre lo stato iniziale
%  E' il principale

pred vai(stato, list(evento)).
%  vai(+S, -History) det
%  vai(S, H) : H è la storia dell'agente a partire da S fino a
%  terminazione

%%	B)  CONOSCENZA

pred conosce(assunzione).
%    conosce(?A) nondet,dinamico
%    conoscenza vera nell'ambiente
pred assunto(assunzione).
%    assunto(?A) nondet, dinamico
%    assunzione fatta ma non verificata
pred pensa(any, list(assunzione)).
%    pensa(+BODY, -Ass) semidet
%    l'agente pensa che BODY sia vero in base a eventuali assunzioni Ass
%    da usare se il vostro agente non ha bisogno di stato e storia
%    per decidere cosa assumere (vedi decide_di_assumere in vai_if.pl)
pred pensa(any, any, list(assunzione), list(assunzione)).
%    pensa(+Stato, +Storia, +A, -Ass) semidet
%    come sopra, da usare se il vostro agente sa ulteriori informazioni
pred impara(assunzione).
%    impara(+A) det
%    L'agente ha verificato A nell'ambiente; lo aggiunge
%    a ciò che conosce ed elimina ogni eventuale assunzione
%    su A


/**** I SEGUENTI SONO PRINCIPLAMENTE DI USO INTERNO di vai.pl
      e non per il vostro progetto                         ****************/

pred esegui(stato, list(evento), decisione, list(decisione),
	    stato, list(evento)).
%  esegui(+S1, +Storia1, +D, +P, -S2, -Storia2) det
%  S2 è lo stato ottenuto eseguendo il piano P per la decisione D e
%  Storia2 aggiunge a Storia1  gli eventi rilevanti occorsi

pred assume_o_conosce(assunzione, list(assunzione), list(assunzione)).
%     assume_o_conosce(+A, +Ass1, -Ass2) semidet
%     l'agente conosce A e non modifica le assunzioni (Ass1=Ass2)
%     se non lo conosce, lo assume e ricorda l'assunzione se decide di
%     farlo

pred assume_o_conosce(any, assunzione, list(assunzione), list(assunzione)).
%    come sopra per un agente che usa ulteriori informazioni

pred pensa(any, list(assunzione), list(assunzione)).
pred inconsistenti(assunzione,assunzione).


/******************   PER DEBUGGING *********************/

type [ricorda(stato, list(evento), evento, list(evento)),
      mostra_decisione(stato, list(evento), decisione),
      mostra_piano(stato, list(evento), decisione, list(decisione)),
      stop(stato,list(evento))]: debug_info.
pred trace_debug(debug_info).


/************** Esclusi dal typechecking ***************/
ignored(strategy(_)).
ignored(debug_on).
ignored(clear_counts).
ignored(ask).
ignored(inc_debug_c).
ignored(command).
ignored(check_ground(_)).
ignored(d_stop(_,_)).
ignored(d_mostra_start(_)).
ignored(d_mostra_stato(_)).
ignored(d_mostra_storia(_)).
ignored(d_mostra_evento(_)).
ignored(d_mostra_conoscenza).
ignored(d_mostra_assunzioni).
ignored(d_mostra_decisione(_,_,_)).
ignored(d_mostra_piano(_,_,_,_)).


