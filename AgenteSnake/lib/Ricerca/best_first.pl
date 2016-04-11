:- consult(frontiera_ord).
:- op(1199, fx, pred).

%%%  STRATEGIA BEST FIRST

pred h(nodo_problema, number).
%    h(+N, -H) :   H e' il valore euristico di N
%    DEVE ESSERE IMPLEMENTATO DAL PROBLEMA

pred leq(nodo_problema, nodo_problema).
%  leq(+N1, +N2) is semidet:  ordinamento della frontiera da usare in
%  frontiera_ord.pl
%  best first ordina la frontieranin base ad h(N)

pred priority(nodo_completo, number).
%    priority(+N,-P):   P = h(N)
%
leq(nc(N1,_,_),nc(N2,_,_)) :-
       h(N1, W1),
       h(N2, W2),
       W1 =< W2.

priority(nc(N,_,_), P) :-
	h(N,P).
