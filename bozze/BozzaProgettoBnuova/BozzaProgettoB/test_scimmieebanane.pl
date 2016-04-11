/*******   CASI DI TEST PER scimmie e banane,anche parallelo;
           usa transition, implementato da entrambi ************/

run_test(trovato,stato_finale:SF) :-
	show_test_case(trovato, _:statofinale, SF),
	(   trovato(SF) -> writeln('OK, trovato vero nello stato finale');
	    writeln('ERRORE, trovato falso nello stato finale')).
run_test(euristica(K), h(S)=H) :-
	show_test_case(euristica, K:Label, S),
	h(S,H),
	maplist(write, [K:Label,'  ', S,'\n valore euristico ',H,'\n']),
	( (Label=statofinale, H \= 0) ->
	  writeln('ERRORE, H dovrebbe essere 0 per lo stao finale')
	  ;
	  true
	).
run_test(add_del(K,Len),A) :-
	(   not(number(Len)) -> Len=1 ; true),
	show_test_case(add_del, K:Label,S),
	maplist(write, ['TEST: ', K:Label, ', n.passi: ', Len, '\n']),
	test(transition(A), S, Len).

show_test_case(Pred, TC, S) :-
	test_case(TC,S),
	nl,
	maplist(write, ['PRED:', Pred, '   TEST CASE: ',TC, '\n']).


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
	list_to_ord_set([giu(sc1), giu(sc2), fame(sc1), fame(sc2), aposto(sd1),
	                 appesa(b1), appesa(b2)], S).

test_case(1:stato1,S) :-
	list_to_ord_set(
	    [aposto(sd1),appesa(b1),appesa(b2),fame(sc1),fame(sc2),giu(sc1),
	     giu(sc2),ha(sc2,sd1)], S).

test_case(2:stato5, S) :-
	list_to_ord_set(
	    [appesa(b1),fame(sc1),giu(sc1),giu(sc2),sazia(sc2),ha(sc2,sd1),
	     sotto(sd1,b1)], S).

test_case(3:statofinale, S):-
	list_to_ord_set(
	[aposto(sd1),giu(sc1),giu(sc2),sazia(sc1),sazia(sc2),ha(sc1,sd1)],S).
