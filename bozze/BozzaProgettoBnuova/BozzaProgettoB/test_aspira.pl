/*******   CASI DI TEST PER aspira ************/
ignored(run_test(_,_)).

run_test(add_del(TC,Len),Act) :-
	(   not(number(Len)) -> Len=1 ; true),
	show_test_case(add_del, TC,S,Len),
	test(transition(Act), S, Len).

show_test_case(Pred, TC, S,Len) :-
	test_case(TC,S),
	nl,
	maplist(write,
		['PRED:', Pred, '   TEST CASE: ',TC, ', n-passi: ', Len, '\n']).


%  sequenze di transizioni lunghe K a partire dallo stato S1;
%  testano add_del,richiamato da transition
test(transition(A), S1, K) :-
	test(transition(A), S1, 0, K).

test(transition(A), S1, H, K) :-
	H < K, !,
	H1 is H+1,
	writeln('PASSO ':H),nl,
	transition(A, S1, S2, C),
	mostra_transizione(A,S1,S2, C),
	test(transition(_), S2, H1, K).
test(_,_,_,_).

mostra_transizione(A,S1,S2,C) :-
	maplist(write, ['AZIONE ', A, ', costo ', C, '\n']),
	writeln('       DA ': S1),
	writeln('       A  ': S2).

/*** CASI DI TEST CON UNA SEDIA *****/

test_case(0:stato_iniziale, S) :-
	starting_state(S).

test_case(1:raccoglitutto,S) :-
	capacita(Max),
	I is Max - 1,
	between(I,Max,S1),
	list_to_ord_set(
	    [in(s1), spazio(Max), sporco(s1, S1), sporco(s2, 5),
	    sporco(s3,3), sporco(s4, 4)], S).
test_case(1:non_hai_spazio,S) :-
	capacita(Max),
	S1 is Max + 2,
	list_to_ord_set(
	    [in(s1), spazio(Max), sporco(s1, S1), sporco(s2, 5),
	    sporco(s3,3), sporco(s4, 4)], S).

test_case(2:svuota, S) :-
	member(Q, [3,0]),
	list_to_ord_set(
	    [in(d), spazio(Q), sporco(s1, 0), sporco(s2, 0),
	    sporco(s3,1), sporco(s4, 4)], S).

test_case(3:statofinale, S):-
	capacita(Max),
	list_to_ord_set(
	[in(d), spazio(Max), sporco(s1, 0), sporco(s2, 0),
	    sporco(s3,0), sporco(s4, 0)],S).
