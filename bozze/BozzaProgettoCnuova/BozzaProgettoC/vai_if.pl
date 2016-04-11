:- module(vai_if, []).
%  INTERFACCIA DI vai.pl da implementare dal progetto

:- use_module(library(is_a)).
:- discontiguous(pred(_)).
:- discontiguous(type(_)).

%  TIPI APERTI, DA DEFINIRE NEL PROGETTO
type stato.       % gli stati mondo-agente
type decisione.   % le decisioni possibili dell'agente
type info_inizio. % informazioni da fornire all'inizio storia
type assumibile.  % metatipo per i predicati assumibili


% PREDICATI APERTI, DA IMPLEMENTARE NEL PROGETTO con in significato qui
% indicato

/***  A) Usati da vai nel ciclo decisione-pianificazione-azione ***/

pred stato_iniziale(stato, info_inizio).
    %   stato_iniziale(-S, +Info) det
    %   S è lo stato iniziale in base ad Info
pred fine(decisione).
    %   fine(+D) semidet
    %   D è la decisione che la storia finisce qui
pred decidi(stato,list(evento),decisione).
    % decidi(+S, +History, -Dec) det
    % l'agente decide Dec trovandosi nello stato S e usando gli eventi
    % memorizzati nella storia precdedente
pred esegui_azione(stato, list(evento), decisione, stato).
    %  esegui_azione(+S1, +Storia, +Azione, -S2) semidet
    %  si ha una transizione di stato S1 --Azione--> S2 possibile nel
    %  mondo;
    %  se l'esecuzione dell'azione pianificata risulta impossibili fallisce

%%	NB : aggiorna_conoscenza non è da implementare se si implementa
%	diversamente  ricorda  (vedi punto D)
pred aggiorna_conoscenza(stato, list(evento), evento).
    %   aggiorna_conoscenza(+S, +Storia1, +E) det:
    %   la base di conoscenza dinamica dell'agente è aggiornata

%%	NB : azione e piano non sono da implementare se si implementa
%	diversamente  pianifica (vedi punto D)
pred azione(decisione).
    % azione(+Az)  semidet
    %  Az è un'azione, cioè una decisione immediatemente eseguibile
    %  NB.Le decisioni NON eseguibili richiedono una pianificazione
pred piano(stato,list(evento),decisione, list(decisione)).
    %   piano(+S, +History, +Dec, -Piano)  semidet
    %   Dec non è eseguibile (in S,History) e Piano è una lista di decisioni
    %   eseguibili che porta a compimento quanto stabilit

/**  B)  Usati da vai nella parte di ragionamento  **/

pred assumibile(any).
%    assumibile(?P) non det:  P è un'atomica assumibile
pred meta(any).
%    meta(?P) nondet:  P è da metainterpretare nella
%    valutazione della conoscenza dell'agente (metapredicato sa).
%    NB!!   Un meta_predicato può essere chiamato SOLO da un altro
%    metapredicato; in caso contrario non verrà metainterpretato
pred decide_se_assumere(assunzione).
%    decide_di_assumere(+A) semidet
%    PRECONDIZIONE:   l'agente non conosce A e non conosce non(A)
%    Se vera nel mondo, A viene assunta nella base conoscenza dinamica
%    ed eventuali assunzioni contrarie ad A vengono eliminate; se falsa
%    non ci sono modifiche nella base di conoscenza dinamica
pred decide_se_assumere(any, assunzione).
%    decide_se_assumere(+Info, +A) semidet
%    Come sopra, ma per decidere serve anche Info
pred contraria(assunzione, assunzione).
%    contraria(+A1,+A2) semidet:
%    A1 /\ A2 è falso, ovvero A1 dice il contrario di A2


/***   C) DA DEFINIRE se si vuol personalizzare le stampe di debug
 *	   e la traccia dell'esecuzione (il "filmato")	 *****/

pred mostra_stato(stato).
pred mostra_stato_iniziale(stato).
pred mostra_transizione_stato(stato,decisione,stato).
pred mostra_storia(list(evento)).
pred mostra_evento(evento).
pred mostra_conoscenza.
pred mostra_assunzioni.
pred mostra_decisione(stato, list(evento), decisione).
pred mostra_piano(stato, list(evento), decisione, list(decisione)).
pred stop(stato,list(evento)).

%  si può intervenire più radicalmente sul "filmato" sostituendo per
%  intero i predicati:

pred mostra_start(stato).
pred mostra_transizione(stato,decisione,stato).

/****** D)  PERSONALIZZAZIONI PIU' GENERALI:
        PREDICATI DA RISCRIVERE SE OPPORTUNO NEL PROGETTO  *****/

pred pianifica(stato, list(evento), decisione, list(decisione)).
    %  pianifica(+S, +H, +D, -P) semidet
    %  P è il piano attuativo di D con stato iniziale S e storia H
    %  Se non vi sono piani possibili, fallisce
/** NB se non lo riscrivete dovete implementare azione e piano come
 * specificato sopra */

pred ricorda(stato, list(evento), evento, list(evento)).
%   ricorda(+S, +Storia1, +E, -Storia2) det
%   Storia2 ricorda (se necessario) l'evento E
%   e la eventuale base di conoscenza dinamica dell'agente è aggiornata

/** NB se non lo riscrivete dovete implementare aggiorna_conoscenza come
 * specificato sopra */
