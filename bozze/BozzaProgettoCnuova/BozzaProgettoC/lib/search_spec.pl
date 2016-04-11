:- module(search_spec,[]).
:- use_module(search_if).


type(T) :- search_if:type(T).
type([nc(TNP,list(TNP),number)]:nodo(TNP)).
type([trovato]:pred(_)). %metatype for call

pred(T) :- search_if:pred(T).
pred(target(stato)).
pred(is_target(pred(stato))). % dinamico da non ridefinire
  %  target(+G) semidet
  %  Vero sse G è un goal; se si usa solve/2 e trovato/1 è definito dal
  %          problema, target(G) equivale a trovato(G);
  %	     se si usa solve/3 e call(Trovato,G), equivale a
  %          call(Trovato,G); is_target memorizza il parametro Trovato

pred(solve(stato, nodo(stato))).
  % solve(+Start,nc(-Goal,-Path,-Cost)) is nondet:   da Start si raggiunge
  %    Goal attraverso il cammino Path con costo Cost; Goal e' una
  %    soluzione

pred(solve(stato, nodo(stato), pred(stato))).
  % solve(+Start, nc(-Goal,-Path,-Cost)m, +Trovato) is nondet:
  %   Path cammino da nodo start a Goal  e call(Trovato,Goal) è vero

ignored(init_counters).
ignored(frontiera_iniziale(_, _, _)).
ignored(cerca(_, _, _, _)).
