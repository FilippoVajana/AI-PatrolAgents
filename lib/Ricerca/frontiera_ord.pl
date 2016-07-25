%--------------------------------------------------------------------


% USA il predicato APERTO
%
% pred priority(nodo(TNP), number)
%    la priorità è a seconda della strategia g, h oppure f
%
% DEFINISCE i predicati usati da "cerca" (file searchmr.pl)
% usando la libreria heaps di SWI prolog
%
%
%----------------------------------------------------------------------

frontiera_vuota(heap(nil,0)).

%% La frontiera iniziale di cerca, VEDI la specifica in searchmr.pl
%
frontiera_iniziale(N,F1) :-
	retractall(chiuso(_)),
	priority(N,P),
	add_to_heap(heap(nil,0), P, N, F1).
frontiera_iniziale(N,F1,E) :-
	priority(N,P),
	retractall(chiuso(_)),
	empty_assoc(E),
	add_to_heap(heap(nil,0), P, N, F1).

scelto(N, F1, F2) :-
	get_from_heap(F1, _, N, F2).

aggiunta([],F,F).
aggiunta([N|V], F, F1) :-
      priority(N,P),
      add_to_heap(F,P,N,FF),
      aggiunta(V,FF,F1).

ins(N, F1, F2):-
	priority(N,P),
	add_to_heap(F1,P,N,F2).

%	%%%%%%%%%%%%%%%%%%%%%%%%%%   PER visualizzare
%       in fase di debugging


mostra(heap(_,0)) :- !.
mostra(H1) :-
    get_from_heap(H1, _P, PN, H2),
    catch(mostra_nodo(PN),_, writeln(PN)),
    mostra(H2).














