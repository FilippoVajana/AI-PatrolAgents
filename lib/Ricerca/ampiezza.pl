
%-----------------------------------------------------------------
%frontiera realizzata come una difference list (dl) per appendere
%in fondo
%
%  type  [dl(list(TN),list(TN))]:frontiera(TN).
%
%  nel contesto chiamata vale:   TN:nodo(TNP)
%  type nodo(TNP) := nc(TNP,list(TNP),number).
% ----------------------------------------------------------------


frontiera_iniziale(N,dl([N|L],L),E):-
	empty_assoc(E),
	retractall(chiuso(_)).

scelto(N,dl(U,V),dl(UU,V)) :-
	not(var(U)),
	U=[N|UU].

aggiunta(VN,dl(F,G),dl(F,NF)) :-
	to_dif_list(VN,dl(FN,LN)),
	concat_dl(dl(F,G), dl(FN,LN), dl(F,NF)).

to_dif_list([],dl(F,F)).
to_dif_list([X|R],dl([X|F],G)) :-
	to_dif_list(R,dl(F,G)).

concat_dl(dl(A,B),dl(B,C),dl(A,C)).

%---------------------------------------------------------------
%  usato in fase di debugging per mostrare la frontiera
%  corrente; usa mostra_nodo, di default una writeln;
%  mostra_nodo pu� essere sovrascritto
mostra(dl(L1,_)) :-
	var(L1), !.
mostra(dl([N|L1],L2)) :-
	mostra_nodo(N),
	mostra(dl(L1,L2)).






