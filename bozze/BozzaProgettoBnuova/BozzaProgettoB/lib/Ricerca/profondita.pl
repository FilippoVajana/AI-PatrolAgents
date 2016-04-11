
%-----------------------------------------------------------------
%frontiera realizzata come una lista
%
%  type frontiera(TN) := {list(nodo(TNP))}.
%
%  nel contesto chiamata vale:   TN:nodo(TNP)
%  type nodo(TNP) := nc(TNP,list(TNP),number).
% ----------------------------------------------------------------


frontiera_iniziale(N,[N]).
frontiera_iniziale(N,[N],E) :-
	empty_assoc(E).

scelto(N,[N|F],F).

aggiunta([],F,F).
aggiunta([N|V], F, [N|F1]) :-
	aggiunta(V,F,F1).

%---------------------------------------------------------------
%  usato in fase di debugging per mostrare la frontiera
%  corrente; usa mostra_nodo, di default una writeln;
%  mostra_nodo può essere sovrascritto

mostra(L) :-
	maplist(mostra_nodo,L).


