
%  best_eq(N1,N2,N3) (usata solo da frontiera_ord) di default e'
%  disattivata, fallisce:
best_eq(_,_,_) :- fail.


% scelta alternativa poto in frontiera e taglio i cicli
% VEDI pota_chiusi per la specifica
%best_eq(nc(N1,P1,C1),nc(N2,P2,C2), nc(N2,P,C)) :-
%	N1=N2,!,
%	(   C1 < C2 -> P=P1, C=C1; P=P2, C=C2),
%	( showflag -> writeln(best_eq(N1:C1, N2:C2, nc(N2,P,C))); true).

%  verifica che il nodo non faccia gi� parte del cammino
taglia(N,L) :-
	in(N,L),
	(	showflag -> writeln(tagliato:N); true).
in(N,[M|_]) :-
	equals(N,M), !.
in(N,[_|L]) :-
	in(N,L).

chiudi(_).   %  non chiude i nodi
chiusura(_,_,_).

potatura(nc(V,Path,_),_,dl(L,L)) :-
	member(V,Path),!.
potatura(N,_,dl([N|L],L)).

/************************** DA ADATTARE AL PROBLEMA **********/

starting.

equals(X,Y) :- X = Y.













