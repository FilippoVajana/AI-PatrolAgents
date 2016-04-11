:- discontiguous(type(_)).
:- discontiguous(pred(_)).
:- use_module(library(mr)).
:- use_module(library(is_a)).
:- use_module(library(action_spec)).
:- consult(library(action)).




type T :- action_spec:type(T).
pred P :- action_spec:pred(P).

gen [{list(fluent)}]: stato.

%  PARTE STATICA

type [s1,s2,s3,s4,d]: locale.
   % i locali, d è il deposito
pred comunica(locale,locale).
   % le stanze in comunicazione
pred capacita(number).
   % la capacità massima del robot

capacita(10).

comunica(S1,S2) :-
	da_a(S1,S2); da_a(S2,S1).
da_a(d,s1).
da_a(s1,s2).
da_a(s1,s4).
da_a(s2,s3).


% I FLUENTI E LE AZIONI

gen  [in(locale),           % in(L): il robot si trova in L
      spazio(number),       % spazio(K): ha ancora spazio per K
      sporco(locale,number) % sporco(L,H): H è lo sporco in L
     ]: fluent.

gen  [va(locale),
      raccoglie(number),
      versa(number)
     ]: action.

pred models(stato, list(fluent)).
%  models(+S, ?L) nondet:  S |= L, dove L è
%  una lista;
%  consente di abbreviare  (member(F1,ST),...,member(Fn,ST))
%  in  models(ST, [F1,...,Fn])
models(S,[F|L]) :-
	member(F,S),
	models(S,L).
models(_S,[]).


starting_state(S0) :-
	capacita(Max),
	S1 is random(Max),
	S2 is random(Max),
	S3 is random(Max),
	S4 is random(Max),
	list_to_ord_set(
	    [in(d), spazio(Max), sporco(s1, S1), sporco(s2, S2),
	    sporco(s3,S3), sporco(s4, S4)], S0).

trovato(ST) :-
	not((models(ST, [sporco(_,S)]), S>0)),
	capacita(Max),
	models(ST, [in(d),spazio(Max)]).

add_del(va(L), ST, [in(L)], [in(LPrec)], 1) :-
	  models(ST, [in(LPrec), spazio(K)]),
	  comunica(LPrec,L),
	  capacita(Max),
	  (
	      LPrec = d ->
	      %  si muove da d solo se è vuoto
	      K=Max,
	      comunica(d,L)
	      ;
	      % se è pieno deve tornare a d
	      K=0 ->
	      true
	      ;
	      % se non è pieno deve aver pulito
	      models(ST, [sporco(LPrec, 0)])
	  ).

add_del(raccoglie(Q), ST, [sporco(S,S2), spazio(K2)],
	               [sporco(S,S1), spazio(K1)], Q) :-
	models(ST, [in(S), sporco(S,S1), spazio(K1)]),
	K1 >0,
	S1 >0,
	Q is min(K1, S1),
	% Q = quantità di sporco raccolta = costo dell'azione
	S2 is S1-Q,
	K2 is K1-Q.

add_del(versa(Q), ST, [spazio(Max)],
	               [spazio(K1)], Q) :-
	capacita(Max),
	models(ST, [in(d), spazio(K1)]),
	Q is Max-K1,
	Q > 0.

h(_,0).
% per così poche stanze non serve una gran euristica;
% comunque provate a pensarne una e confrontatela con
% l'euristica 0 attivando le statistiche di mr

/*************  TESTING *****************/

:- consult(test_aspira).

