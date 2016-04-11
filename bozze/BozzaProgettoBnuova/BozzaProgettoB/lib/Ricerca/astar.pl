:- consult(frontiera_ord).
:- op(1199, fx, pred).

%%%  STRATEGIA A*:  ordina frontiera_ord in base a f(NP) = g(NP) + h(NP)
%

pred h(nodo_problema, number).
%    pred h(+N, -H) is det:   H = h(N) (valore euristico di N)
%    DEVE ESSERE DEFINITO DAL PROBLEMA

pred priority(nodo_completo, number).
%    priority(+N,-P):   P = f(N) = g(N)+h(N)

pred leq(nodo_problema, nodo_problema).
%  leq(+N1, +N2) is semidet:  ordinamento della frontiera da usare in
%  frontiera_ord.pl
%  per A* la frontiera e' ordinata in base a f(N) = g(N) + h(N)
%
leq(nc(N1,_,Costo1),nc(N2,_,Costo2)) :-
       h(N1, W1),
       h(N2, W2),
       Costo1+W1 =< Costo2+W2.

priority(nc(N,_,Costo), P) :-
	h(N,W),
	P is Costo + W.
