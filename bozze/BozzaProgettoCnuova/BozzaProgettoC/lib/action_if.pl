:- module(action_if, []).
:- use_module(search_if).

:- discontiguous(type(_)).
:- discontiguous(pred(_)).

type(T) :- search_if:type(T).
pred(T) :- search_if:pred(T).

type(fluent).
type(action).
%  stato_piano si implementa come ordset(fluent)

pred(starting_state(stato)).
%   starting_state(-S) det
%   Spec:  S è lo stato iniziale
pred(trovato(stato)).
%   trovato(?S) nondet
%   Spec:  S è un goal

pred(add_del(action, stato, list(fluent), list(fluent), number)).
%   add_del(?A, +S, ?Add, ?Del, ?Cost) nondet
%   Spec:  A eseguibile in S con costo Cost e effetto (Add,Del)

pred(h(stato, number)).
%  h(+S, -H) det:  H = valore euristico di S
