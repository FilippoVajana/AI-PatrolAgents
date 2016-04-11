:- module(action_spec, []).
:- use_module(search_spec).
:- use_module(action_if).

:- discontiguous(type(_)).
:- discontiguous(pred(_)).

type(T) :- search_spec:type(T).
type(T) :- action_if:type(T).
pred(T) :- search_if:pred(T).
pred(T) :- action_if:pred(T).

pred(transition(action, stato, stato, number)).
%   transition(?A, +S1, ?S2, ?Cost) nondet
%   Spec:  A manda S1 in S2 con costo Cost

pred(transition(stato, stato)).
%   transition(+S1, ?S2) nondet
%   Spec:  da S1 si passa a S2 con un'azione

pred(states_to_actions(list(stato), list(action))).
%   states_to_actions(+LS, -LA) semidet
%   Se  LS è una lista di stati percorribile, LA è la
%   lista delle azioni che la percorre
%   Se non è percorribile, fallisce

pred(solution(list(action),number)).
%   solution(-LA, -C) semidet
%   Dec:  LA è una soluzione con costo C

pred(mostra_nodo(nodo(stato))).
%  comando, stampa un nodo come sequenza di azioni

pred(exec).
% comando, calcola la prima soluzione, la visualizza
% e poi mostra l'esecuzione del piano dato dalla soluzione

pred(exec(stato, list(action))).
%  exec(+S, +Piano) det: comando, mostra l'esecuzione
%  di Piano a partire da S

pred(default_strategy).
%  comando, carica astar con pota_chiusi
%
/*****************  non importante per il type checking ****/

pred(last_solution(any,any,any)).

