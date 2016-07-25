:- module(search_if, []).

type([[], [T|list(T)]]: list(T)).
type(stato).
type(number).

%%%%%%%% PREDICATI DEFINITI DAL PROBLEMA

pred(trovato(stato)).
   %  trovato(+N) is semidet:    N e' un goal, dipende dal problema
pred(vicini(stato, list(stato))).
   % vicini(+N, -L) is det:   L e' la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred(costo(stato, stato, number)).
   % costo(+N1,+N2,-C) is det: C e' il costo dell'arco (N1,N2)

pred(h(stato, number)).
   % h(+N,-H) is det:  N e' il nodo corrente, H e' la stima
   % euristica del costo da N ad una soluzione ottimale
