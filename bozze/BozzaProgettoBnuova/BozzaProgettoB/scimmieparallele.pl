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

%  AZIONI PARALLELE: usano action1 = azioni che una scimmia può fare,
%  senza indicare la scimmia

type [ sale(sedia), scende,
      sposta(sedia,banana), ripone(sedia),
      mangia(banana), wait,
      prende(sedia), lascia(sedia)]: action1.

gen [par(action1, action1)]: action.
%  par(A1,A2)  : sc1 fa A1, sc2 fa A2 in parallelo; il tempo è il
%  massimo dei 2 (per semplificare si ha una specie di sincronizzazione)

pred member(fluent,stato). %fluente vero in uno stato
pred scimmia(scimmia). % predicato di tipo
pred sedia(sedia).  % predicato di tipo
pred conflittuali(action1, action1).
%   conflittuali(?A1, ?A2) nondet:  A1, A2 possono confliggere
pred conflitto(action1, action1).
%   conflitto(+A1, +A2) semidet:  A1, A2 sono in conflitto
pred non_conflittuale(action1).
%   non_conflittuale(?A) nondet: non vi sono azioni che possono
%   confliggere con A

scimmia(sc1).
scimmia(sc2).
sedia(sd1).
% ---> Commentare la riga seguente per  1 sedia
sedia(sd2).


starting_state(S0) :-
	list_to_ord_set(
	    [giu(sc1), giu(sc2), fame(sc1), fame(sc2), aposto(sd1),
% ---> Commentare la riga seguente per  1 sedia
	     aposto(sd2),
	     appesa(b1),appesa(b2)], S0).

trovato(S) :- member(sazia(sc1),S), member(sazia(sc2),S),
% ---> Commentare la riga seguente per  1 sedia
	member(aposto(sd2),S),
	member(aposto(sd1),S).


conflittuali(prende(_),prende(_)).
conflittuali(sposta(_,_), sposta(_,_)).

conflitto(prende(Sd), prende(Sd)).
conflitto(sposta(_,B), sposta(_,B)).

non_conflittuale(sale(_)).
non_conflittuale(scende).
non_conflittuale(ripone(_)).
non_conflittuale(mangia(_)).
non_conflittuale(lascia(_)).
non_conflittuale(wait).

add_del(par(A1,A2), St, Add,Del, C) :-
	(   add_del1(sc1, A1, St, Add1, Del1, C1),
	    (   conflittuali(A1,A2),
	        add_del1(sc2, A2, St, Add2, Del2, C2),
	        not(conflitto(A1,A2))
	    ;   (non_conflittuale(A1); non_conflittuale(A2)),
	        add_del1(sc2, A2, St, Add2, Del2, C2)
	    )
	) *->
	union(Add1, Add2, Add),
	union(Del1, Del2, Del),
	C is max(C1,C2)
	;
	(
	add_del1(sc1, A1, St, Add, Del, C),
	A2 = wait
	;
	add_del1(sc2, A2, St, Add, Del, C),
	A1 = wait
	).


add_del1(Sc, sale(Sd), St, [su(Sc,Sd)], [giu(Sc)], 1) :-
	member(giu(Sc), St),
	member(ha(Sc,Sd),St).
add_del1(Sc, scende, St, [giu(Sc)], [su(Sc,Sd)], 1) :-
	member(su(Sc,Sd), St).
add_del1(Sc, sposta(Sd,B), St, [sotto(Sd,B)], [aposto(Sd)], 2) :-
	member(appesa(B), St),
	member(aposto(Sd), St),
	member(giu(Sc), St),
	member(ha(Sc,Sd), St),
	not(member(sotto(_,B),St)).
add_del1(Sc, sposta(Sd,B1), St, [sotto(Sd,B1)], [sotto(Sd,B2)], 2) :-
	member(appesa(B1), St),
	member(sotto(Sd,B2), St),
	member(giu(Sc), St),
	member(ha(Sc,Sd), St),
	not(member(sotto(_,B1),St)).
add_del1(Sc, ripone(Sd), St, [aposto(Sd)], [sotto(Sd,B)], 2) :-
	member(sotto(Sd,B), St),
	member(giu(Sc), St),
	member(ha(Sc,Sd), St).
add_del1(Sc, mangia(B), St, [sazia(Sc)], [fame(Sc), appesa(B)], 1) :-
	member(fame(Sc), St),
	member(su(Sc,Sd), St),
	member(sotto(Sd,B), St),
	member(appesa(B), St).
add_del1(Sc, prende(Sd), St, [ha(Sc,Sd)], [], 1) :-
	sedia(Sd),
	not(member(ha(_,Sd), St)).
add_del1(Sc, lascia(Sd), St, [], [ha(Sc,Sd)], 1) :-
	member(ha(Sc,Sd), St),
	member(giu(Sc), St).


h(ST, H) :-
	size(da_prendere(ST), K3),
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
	H is 3*K3 + 2*H2 + 3*H3 + 4*H4 + 5*H5 + 6*H6.
size(P, H) :-
	setof(X, call(P,X), S), !, length(S,H)
	;
	H = 0.
da_prendere(ST, Sd) :-
	member(sotto(Sd,B), ST),
	not(member(ha(_,Sd), ST)),
	not(member(appesa(B),ST)).
deve_mangiare(ST, S) :-
	member(fame(S), ST),
	member(su(S,Sd), ST),
	member(sotto(Sd,B), ST),
	member(appesa(B), ST).
deve_prendere(ST, S) :-
	member(fame(S),ST),
	member(giu(S), ST),
	not(member(ha(S,_),ST)).
deve_spostare(ST, S) :-
	member(fame(S),ST),
        member(giu(S), ST),
	member(ha(S,Sd1),ST),
	not((member(sotto(Sd1,B),ST), member(appesa(B),ST))).
deve_salire(ST, S) :-
	    member(fame(S),ST),
	    member(giu(S), ST),
	    member(ha(S,Sd1),ST),
	    member(sotto(Sd1,B),ST),
	    member(appesa(B),ST),
	    not(member(su(S,Sd1),ST)).
deve_scendere(ST, Sc) :-
	 member(su(Sc,Sd),ST),
	 member(sazia(Sc),ST),
	 not(member(aposto(Sd),ST)).




