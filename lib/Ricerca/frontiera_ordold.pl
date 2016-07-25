%--------------------------------------------------------------------
%
%  type list(E) := []; [E | list(E)].
%  type frontiera(TNP) :=
%	  o  ;
%         f(nodo(TNP),frontiera(TNP),frontiera(TNP)).
%
% una frontiera e' un albero binario dove:
%     - o  è l'albero vuoto
frontiera_vuota(o).
%     - f(N, S, D) è un albero con radice N: nodo(TNP)
%         S, D sono i due sottoalberi
%     - valgolno le seguenti proprieta' invarianti:
%
% Proprieta' di priorita':   la radice di ogni albero e sottoalbero
%         ne e' il minimo
% Proprieta' di bilanciamento:  per ogni albero e sottoalbero,
% detti ND ed NS i numeri dei nodi dei sottoalberi destro e sinistro,
% vale:    ND = NS  o ND = NS+1
%
% Il bilanciamento garantisce che se T e' il numero totale
% dei nodi di un albero, i cammini hanno lunghezza
% L <= log_2(T)+1
%
% Tempi di inserimento e di estrazione: logaritmici
%
% ------------------------------------------------------------------

%-------------------------------------------------------------------
% USA il predicato APERTO
%
% pred leq(nodo(TNP), nodo(TNP))
%    leq(N1,N2)	 : N1 precede N2 ndll'ordinamento sui nodi definito
%    dalla strategia di ricerca
%
% DEFINISCE i predicati usati da "cerca" (file searchmr.pl)
%    -  frontiera iniziale
%    -  aggiunta
%    -  scelto
% usando i predicati di inserimento ed estrazione in tempo logaritmico:
%    - ins
%    - ext_foglia
%
%----------------------------------------------------------------------


%% La frontiera iniziale di cerca, VEDI la specifica in searchmr.pl
%
frontiera_iniziale(N,f(N,o,o)).

%% La aggiunta dei vicini alla frontiera, mantenendone le
%  proprieta' invarianti relative a ordinamento e bilanciamento;
%  vedi la specifica generale in searchmr.pl
%
aggiunta([],F,F).
aggiunta([N|V], F, F1) :-
      ins(N,F,FF),
      aggiunta(V,FF,F1).


%  La scelta ed estrazione del nodo minimo in frontiera
%  usando l'ordinamento leq fornito dalla strategia
%  vedi la specifica generale in searchmr.pl
%
scelto(X, f(X,o,o), o) :- !.
scelto(N, f(N,S,D), f(N1,S1,D1)) :-
   once(ext_foglia(F, D, DmenoF)),
   once(riempi(F, f(_,DmenoF,S), f(N1,S1,D1))).

%  IMPLEMENTAZIONE DI INSERIMENTO ED ESTRAZIONE:

% pred ins(nodo(TNP), frontiera(TNP), frontiera(TNP))
%     ins(+X,+F,-F1):
%     pre: F frontiera di priorita'
%     post: F1 = F unito X e F1 frontiera priorita'

ins(X, o, f(X,o,o)):- !.
%ins(X, f(N,S,D), f(N1,S,D)) :-
   % escludo duplicati in frontiera, quando
   % applico il taglio dei chiusi; se non attivata,
   % best_eq fallisce
   %best_eq(X, N, N1),!.
ins(X, f(N,S,D), f(N1,D,S1)) :-
   leq(X,N) ->
       (  N1 = X,
          ins(N, S, S1)
        )
       ;
       (  N1 = N,
          ins(X, S, S1)).

% pred ext_foglia(nodo(TNP), frontiera(TNP), frontiera(TNP))
%     ext_foglia(-Foglia, +F, -F1)
%     pre:   F frontiera di priorita'
%     post:  - F1 frontiera di priorità
%            - Foglia e' una foglia di F,
%            - F1 = F meno Foglia,

ext_foglia(F, f(F,o,o), o) :- !.
ext_foglia(F, f(N,S,D), f(N,ND,S)) :-
    ext_foglia(F,D,ND).

% pred riempi(nodo(TNP), frontiera(TNP), frontiera(TNP))
%    riempi(+N, +F, -F1)
%    pre:   F frontiera di priorita' da cui la radice e' stata tolta;
%	    diremo che F ha un "buco", che indicheremo con _ :
%	    F = f(_, S, D)
%    post:  F1 riempie il buco _ di F con N, portandolo al posto
%	    giusto per mantenere l'ordinamento


% caso base1:  F = f(_,o,o), cioe' F contiene solo il buco _,
%              che viene riempito con il nodo N

riempi(N, f(_,o,o), f(N,o,o)).

% caso base 2:   F = f(_,o,f(ND,o,o)), cioe' F contiene il buco e sola
%                una foglia, a destra per il bilanciamento:
%                se N <= ND, riempie il buco,
%                altrimenti lo riempie ND e N va al posto di ND

riempi(N, f(_,o,f(ND,o,o)),f(N, o, f(ND,o,o))) :-
     leq(N,ND), !.
riempi(N, f(_,o,f(ND,o,o)),f(ND, o, f(N,o,o))).


% caso ricorsivo 1:  due sottoalberi con radici N1,N2
%                    e N <= di entrambe:   mettiamo N nel
%                    buco senza chiamata ricorsiva alcuna
riempi(N, f(_,f(N1,S1,D1),
                f(N2,S2,D2)),
	    f(N,f(N1,S1,D1),
                f(N2,S2,D2))) :-
     leq(N,N1),
     leq(N,N2), !.

% caso ricorsivo 2:  due sottoalberi con radici N1,N2
%                    e, per il cut!,  N > di almeno uno fra N1,N2
%		     e, per il test leq(N1,N2), N1 e' il minimo
%		     fra N, N1, N2:  mettiamo N1 nel buco, aprendo
%                    un buco nel sotto-albero di N1;
%                    Ricorsivamente, riempiamo il buco con N in tale
%                    sottoalbero

riempi(N, f(_,f(N1,S1,D1),
                f(N2,S2,D2)),
	    f(N1,RiempitoS,
                f(N2,S2,D2))) :-
     leq(N1,N2), !,
     riempi(N, f(_, S1,D1), RiempitoS).

% caso ricorsivo 3:  due sottoalberi con radici N1,N2
%                    e, per i due cut!,  N2 e' il minimo
%		     fra N, N1, N2:  mettiamo N2 nel buco, aprendo
%                    un buco nel sotto-albero di N2;
%                    Ricorsivamente, riempiamo il buco con N in tale
%                    sottoalbero

riempi(N, f(_,f(N1,S1,D1),
                f(N2,S2,D2)),
	    f(N2,f(N1,S1,D1),
                 RiempitoD)) :-
     riempi(N, f(_, S2,D2), RiempitoD).



%---------------------------------------------------------------
%  usato in fase di debugging per mostrare la frontiera
%  corrente; usa mostra_nodo, definito nel debugger.pl;
%  è implementato con writeln, può essere modificato o
%  sovrascritto

%	%%%%%%%%%%%%%%%%%%%%%%%%%%   PER visualizzare
%       in fase di debugging,
mostra(o) :- !.
mostra(F) :-
     scelto(N,F,F2),
     mostra_nodo(N),
     mostra(F2).

















