:- discontiguous(type(_)).
:- discontiguous(pred(_)).
:- use_module(library(mr)).
:- use_module(library(is_a)).
:- use_module(library(action_spec)).
:- consult(library(action)).



type T :- action_spec:type(T).
pred P :- action_spec:pred(P).

gen [{list(fluent)}]: stato.

type [sc1,sc2]:scimmia.
type [sd1, sd2]:sedia.
type [b1,b2]:banana.

gen [su(scimmia,sedia), giu(scimmia), ha(scimmia,sedia), sotto(sedia, banana),           aposto(sedia), appesa(banana), fame(scimmia), sazia(scimmia)]: fluent.
gen [ sale(scimmia,sedia), scende(scimmia),
      sposta(scimmia,sedia,banana), ripone(scimmia,sedia),
      mangia(scimmia, banana),
      prende(scimmia,sedia), lascia(scimmia,sedia)]: action.


pred models(stato, list(fluent)).
%  models(+S, ?L) nondet:  S |= L, dove L è
%  una lista;
%  consente di abbreviare  (member(F1,ST),...,member(Fn,ST))
%  in  models(ST, [F1,...,Fn])
models(S,[F|L]) :-
	member(F,S),
	models(S,L).
models(_S,[]).

pred scimmia(scimmia).
pred sedia(sedia).

scimmia(sc1).
scimmia(sc2).
sedia(sd1).
sedia(sd2).


starting_state(S0) :-
	list_to_ord_set(
	    [giu(sc1), giu(sc2), fame(sc1), fame(sc2), aposto(sd1),
	     aposto(sd2),
	     appesa(b1),appesa(b2)], S0).

trovato(S) :-
	models(S,[sazia(sc1), sazia(sc2), aposto(sd2), aposto(sd1)]).

add_del(sale(Sc,Sd), St, [su(Sc,Sd)], [giu(Sc)], 1) :-
	models(St, [giu(Sc), ha(Sc,Sd)]).
add_del(scende(Sc), St, [giu(Sc)], [su(Sc,Sd)], 1) :-
	models(St, [su(Sc,Sd)]).
add_del(sposta(Sc,Sd,B), St, [sotto(Sd,B)], [aposto(Sd)], 2) :-
	models(St, [ha(Sc,Sd),appesa(B),aposto(Sd),giu(Sc)]),
	not(models(St, [sotto(_,B)])).
add_del(sposta(Sc,Sd,B1), St, [sotto(Sd,B1)], [sotto(Sd,B2)], 2) :-
	models(St, [ha(Sc,Sd),appesa(B1),sotto(Sd,B2),giu(Sc)]),
	not(models(St, [sotto(_,B1)])).
add_del(ripone(Sc,Sd), St, [aposto(Sd)], [sotto(Sd,B)], 2) :-
	models(St, [sotto(Sd,B), ha(Sc,Sd), giu(Sc)]).
add_del(mangia(Sc,B), St, [sazia(Sc)], [fame(Sc), appesa(B)], 1) :-
	models(St, [fame(Sc), su(Sc,Sd), sotto(Sd,B), appesa(B)]).
add_del(prende(Sc,Sd), St, [ha(Sc,Sd)], [], 1) :-
	scimmia(Sc),
	not(models(St, [ha(Sc,_)])),
	sedia(Sd),
	not(models(St,[ha(_,Sd)])).
add_del(lascia(Sc, Sd), St, [], [ha(Sc,Sd)], 1) :-
	models(St, [ha(Sc,Sd), giu(Sc)]).


h(ST, H) :-
	size(abbandonata(ST), K3),
	  % min: prende e ripone:  3
	size(deve_scendere(ST), H2),
	  % min: scende, lascia:2
	size(deve_mangiare(ST), H3),
	  % minimo:mangia, scende, lascia: 3
	size(deve_salire(ST),H4),
	  % sale, mangia, scende, lascia: 4
	size(deve_spostare(ST), H5),
	  % sposta e poi come sopra:  5
	size(deve_prendere(ST), H6),
	  % prende e poi come sopra: 6
	H is 3*K3 +
	     2*H2 +
	     3*H3 +
	     4*H4 +
	     5*H5 +
	     6*H6.

%  Fatti indesiderati: richiedono passi per arrivare
%  alla soluzione; si valuta il minimo numero di passi
%  con rilassamento dei vincoli
pred abbandonata(stato, sedia).
%  abbandonata(+St, -Sd) nondet:
%  Sd è una sedia abbandonata sotto a una banana mangiata
pred deve_mangiare(stato, scimmia).
%  deve_mangiare(+St, -Sc) det:
%  Sc è su una sedie sotto una banana ed deve mangiarla
pred deve_prendere(stato, scimmia).
%  deve_prendere(+St, -Sc) det:
%  Sc ha fame ma non ha preso nessuna sedia
pred deve_spostare(stato, scimmia).
%  deve_spostare(+St, -Sc) det:
%  Sc ha fame, ha una sedia ma la sedia non è
%  sotto a una banana ancora appesa
pred deve_salire(stato, scimmia).
%  deve_salire(+St, -Sc) det:
%  Sc ha fame, ha una sedia sotto a una banana ma è giu'
pred deve_scendere(stato, scimmia).
%  deve_scendere(+St, -Sc) det:
%  Sc è sazia e ozia sopra a una sedia

%  predicato ausiliario per contare i fatti indesiderati
pred size(any, number).
%   size(+P,-N) det:  N = || {X | P(X)} ||

size(P, H) :-
	setof(X, call(P,X), S), !, length(S,H)
	;
	H = 0.
abbandonata(ST, Sd) :-
	models(ST, [sotto(Sd,B)]),
	not(models(ST, [ha(_,Sd)])),
	not(models(ST, [appesa(B)])).
deve_mangiare(ST, S) :-
	models(ST, [fame(S), su(S,Sd), sotto(Sd,B),appesa(B)]).
deve_prendere(ST, S) :-
	models(ST, [fame(S), giu(S)]),
	not(models(ST, [ha(S,_)])).
deve_spostare(ST, S) :-
	models(ST, [fame(S), giu(S), ha(S,Sd1)]),
	not(models(ST, [sotto(Sd1,B),appesa(B)])).
deve_salire(ST, S) :-
	models(ST, [fame(S), giu(S), ha(S,Sd), sotto(Sd,B),appesa(B)]).
deve_scendere(ST, Sc) :-
	 models(ST, [su(Sc,_Sd),sazia(Sc)]).



/*****************  TESTING  *****************/

:-consult(test_scimmieebanane).

