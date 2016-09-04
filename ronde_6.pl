:-dynamic(stato_corrente/2).

ronda(r1,p(3,3),e,0) :- !.
ronda(r1,p(3,4),e,1) :- !.
ronda(r1,p(3,5),e,2) :- !.
ronda(r1,p(3,5),e,3) :- !.
ronda(r1,p(4,5),s,4) :- !.
ronda(r1,p(5,5),s,5) :- !.
ronda(r1,p(6,5),s,6) :- !.
ronda(r1,p(6,5),o,7) :- !.
ronda(r1,p(5,5),n,8) :- !.
ronda(r1,p(4,5),n,9) :- !.
ronda(r1,p(3,5),n,10):- !.
ronda(r1,p(3,5),n,11):- !.
ronda(r1,p(3,4),o,12):- !.
ronda(r1,p(3,3),o,13):- !.
ronda(r1,p(3,3),s,14):- !.
ronda(r1,P,D,K) :-
  K > 14,
  H is K mod 15,
  ronda(r1,P,D,H).

ronda(r2,p(7,9),o,0):- !.
ronda(r2,p(7,9),o,1):- !.
ronda(r2,p(7,9),o,2):- !.
ronda(r2,p(7,9),o,3):- !,
  retractall(stato_corrente(r2,_)),
  random_between(0,1,0) ->
    assert(stato_corrente(r1,q1))
    ;
    assert(stato_corrente(r1,q2)).

ronda(r2,p(7,9),o,4) :-
  stato_corrente(r2,q1), !.
ronda(r2,p(7,9),o,5) :-
  stato_corrente(r2,q1), !.
ronda(r2,p(7,9),o,6) :-
  stato_corrente(r2,q1), !.
ronda(r2,p(7,9),n,4) :-
  stato_corrente(r2,q2), !.
ronda(r2,p(7,9),n,5) :-
  stato_corrente(r2,q2), !.
ronda(r2,p(7,9),n,6) :-
  stato_corrente(r2,q2), !.

ronda(r2,P,D,K) :-
  K > 6,!,
  H is K mod 7,
  ronda(r2,P,D,H).
